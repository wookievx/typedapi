package typedapi.server.finagle

import cats.effect.Sync
import com.twitter.finagle.http.Request
import com.twitter.io.Buf

import scala.language.higherKinds

trait FinagleDecoder[F[_], T] {
  def decode(request: Request): F[T]
}

object FinagleDecoder {

  implicit def stringDecoder[F[_]](implicit F: Sync[F]): FinagleDecoder[F, String] = req => {
    F.delay {
      Buf.Utf8.unapply(req.content).get
    }
  }

}
