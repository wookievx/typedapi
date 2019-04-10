package typedapi.server.finagle

import com.twitter.finagle.http.HeaderMap

class HeaderMapView private[finagle](headers: HeaderMap) extends Map[String, String] {
  override def +[V1 >: String](kv: (String, V1)): Map[String, V1] = ???
  override def get(key: String): Option[String] = headers.get(key)
  override def iterator: Iterator[(String, String)] = headers.iterator
  override def -(key: String): Map[String, String] = new HeaderMapView(headers - key)
}
