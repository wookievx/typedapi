package typedapi.server.docs.openapi

import com.avsystem.commons.rest.openapi.{Server => _, _}
import org.specs2.mutable.Specification
import typedapi.dsl._
import DocDerivationSuite.RefOrUnwrapOps

class DocDerivationSuite extends Specification {

  case class Foo(name: String, surname: String)

  implicit val fooSchema: RestSchema[Foo] = RestStructure.materialize[Foo].standaloneSchema

  "doc derivation builds correct OpenApi object" >> {

    val Api = := :> "find" :> Segment[String]("name") :>
      Query[Int]("limit") :>
      Client.Header("hello", "world") :>
      Server.Send("foo", "bar") :> Server.Match[String]("hi") :>
      ReqBody[Json, Foo] :>
      Put[Json, List[Foo]]

    val docs = document(Api)

    "where path is correct" >> {
      docs.paths.paths must haveKey("/find/{name}/")
    }

    def operation =
      docs.paths.paths("/find/{name}/")
        .assumeValue.put.toOption

    "where operation put is defined and contains all parameters" >> {
      operation should beSome { op: Operation =>
        op.parameters.exists(_.checkValue(_.name == "name")) &&
        op.parameters.exists(_.checkValue(_.name == "limit")) &&
        op.parameters.exists(_.checkValue(_.name == "hi"))
      }
    }

    "where operation put contains correct request and response" >> {
      operation should beSome { op: Operation =>
        op.requestBody.toOption.isDefined &&
        op.responses.byStatusCode.get(200).exists({ r =>
          val response = r.assumeValue
          response.headers.contains("foo") &&
          response.content.get(MediaTypes.`application/json`.value).isDefined
        })
      }
    }
  }

}


object DocDerivationSuite {

  private[openapi] implicit class RefOrUnwrapOps[T](private val refOr: RefOr[T]) extends AnyVal {
    def assumeValue: T = refOr match {
      case RefOr.Value(v) => v
      case d => throw new IllegalStateException(s"Encountered ref $d, when value was expected")
    }

    def checkValue(f: T => Boolean): Boolean = refOr match {
      case RefOr.Value(v) => f(v)
      case _ => false
    }
  }

}