package typedapi.server.docs.generic

import com.avsystem.commons.rest.openapi.Response

trait StatusOf[T] {
  def status: Int
  def response: Response
}
