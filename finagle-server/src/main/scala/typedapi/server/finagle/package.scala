package typedapi.server

import cats.effect.{Resource, Sync}
import cats.syntax.all._
import cats.{Functor, Monad}
import com.twitter.finagle.Http.Server
import com.twitter.finagle.http._
import com.twitter.finagle.{ListeningServer, Service}
import shapeless._
import shapeless.ops.hlist.Prepend
import typedapi.shared._

import scala.language.higherKinds

package object finagle {

  type ServerBuilder[F[_]] = Resource[F, Server]

  private implicit class RequestOps[F[_]](private val req: Request) extends AnyVal {
    def as[T](implicit decoder: FinagleDecoder[F, T]): F[T] = decoder.decode(req)
  }

  private implicit class ParamMapOps(private val params: ParamMap) extends AnyVal {
    def explicitMap: Map[String, List[String]] = new ParamMapView(params)
  }

  private implicit class HeaderMapOps(private val params: HeaderMap) extends AnyVal {
    def immutable: Map[String, String] = new HeaderMapView(params)
  }

  private def respGenerator[F[_] : Functor, T](code: Status)(response: T, headers: Map[String, String])(implicit
    encoder: FinagleEncoder[F, T]
  ) =
    encoder.encode(response).map { resp =>
      for ((n, v) <- headers) resp.headerMap.addUnsafe(n, v)
      resp.status = code
      resp
    }

  implicit def noReqBodyExecutor[El <: HList, KIn <: HList, VIn <: HList, M <: MethodType, F[_], FOut](implicit
    encoder: FinagleEncoder[F, FOut],
    F: Monad[F]
  ) =
    new NoReqBodyExecutor[El, KIn, VIn, M, F, FOut] {
      type R = Request
      type Out = F[Response]

      def apply(req: R, eReq: EndpointRequest, endpoint: Endpoint[El, KIn, VIn, M, VIn, F, FOut]): Either[ExtractionError, Out] = {
        extract(eReq, endpoint).map { extracted =>
          F.flatMap(execute(extracted, endpoint)) {
            case Right((code, response)) =>
              respGenerator(Status(code.statusCode))(response, endpoint.headers)

            case Left(HttpError(code, msg)) =>
              respGenerator(Status(code.statusCode))(msg, endpoint.headers)
          }
        }
      }
    }

  implicit def withReqBodyExecutor[El <: HList, KIn <: HList, VIn <: HList, Bd, M <: MethodType, ROut <: HList, POut <: HList, F[_] : Monad, FOut]
  (implicit encoder: FinagleEncoder[F, FOut],
    decoder: FinagleDecoder[F, Bd],
    _prepend: Prepend.Aux[ROut, Bd :: HNil, POut],
    _eqProof: POut =:= VIn) = new ReqBodyExecutor[El, KIn, VIn, Bd, M, ROut, POut, F, FOut] {
    type R = Request
    type Out = F[Response]

    implicit val prepend = _prepend
    implicit val eqProof = _eqProof

    def apply(req: R, eReq: EndpointRequest, endpoint: Endpoint[El, KIn, VIn, M, (BodyType[Bd], ROut), F, FOut]): Either[ExtractionError, Out] = {
      extract(eReq, endpoint).map { case (_, extracted) =>
        for {
          body <- req.as[Bd]
          response <- execute(extracted, body, endpoint).flatMap {
            case Right((code, response)) =>
              respGenerator(Status(code.statusCode))(response, endpoint.headers)

            case Left(HttpError(code, msg)) =>
              respGenerator(Status(code.statusCode))(msg, endpoint.headers)
          }
        } yield response
      }
    }
  }

  implicit def mountEndpoints[F[_]](implicit
    F: Sync[F],
    run: UnsafeRun[F]
  ) = new MountEndpoints[ServerBuilder[F], Request, F[Response]] {

    type Out = Resource[F, ListeningServer]

    def apply(server: ServerManager[ServerBuilder[F]], endpoints: List[Serve[Request, F[Response]]]): Out = {
      val service: Service[Request, Response] = Service.mk { request =>
        import scala.collection.immutable.::
        def execute(eps: List[Serve[Request, F[Response]]], eReq: EndpointRequest): F[Response] = eps match {
          case endpoint :: tail => endpoint(request, eReq) match {
            case Right(response) => response
            case Left(RouteNotFound) => execute(tail, eReq)
            case Left(BadRouteRequest(msg)) => respGenerator(Status.BadRequest)(msg, Map.empty)
          }

          case Nil => respGenerator(Status.NotFound)("uri = " + request.uri, Map.empty)
        }

        val eReq = EndpointRequest(
          request.method.name,
          {
            val path = request.path.split("/")
            if (path.isEmpty) List.empty
            else path.tail.toList
          },
          request.params.explicitMap,
          request.headerMap.immutable
        )

        if (request.method.name == "OPTIONS") {
          run.runAsTwitterFuture(respGenerator(Status.NoContent)("", optionsHeaders(endpoints, eReq)))
        } else {
          run.runAsTwitterFuture(execute(endpoints, eReq))
        }
      }

      val address = s"${server.host}:${server.port}"
      for {
        s <- server.server
        ls <- Resource.liftF(F.delay(s.serve(address, service)))
      } yield ls
    }
  }


}
