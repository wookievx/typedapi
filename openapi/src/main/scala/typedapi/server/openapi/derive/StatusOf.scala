package typedapi.server.openapi.derive

import com.avsystem.commons.rest.openapi.Response

trait StatusOf[T] {
  def status: Int
  def response: Response
}
