package typedapi.server.finagle

import cats.Applicative
import com.twitter.finagle.http.{MediaType, Response, Status}

import scala.language.higherKinds

trait FinagleEncoder[F[_], T] {
  def encode(t: T): F[Response]
}

object FinagleEncoder {

  implicit def stringEncoder[F[_]](implicit ap: Applicative[F]): FinagleEncoder[F, String] = sbody => ap.pure {
    val response = Response(Status.Ok)
    response.mediaType = MediaType.PlainText
    response.contentString = sbody
    response
  }


}
