package typedapi.server.finagle

import com.twitter.util.Future

import scala.language.higherKinds

trait UnsafeRun[F[_]] {
  def runAsTwitterFuture[T](code: F[T]): Future[T]
}

object UnsafeRun {
  def apply[F[_]](implicit ur: UnsafeRun[F]): UnsafeRun[F] = ur
}
