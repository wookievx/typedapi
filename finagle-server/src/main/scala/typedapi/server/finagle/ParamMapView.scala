package typedapi.server.finagle

import com.twitter.finagle.http.ParamMap

class ParamMapView(paramMap: ParamMap) extends Map[String, List[String]] {
  override def get(key: String): Option[List[String]] = {
    val underlying = paramMap.getAll(key)
    if (underlying.isEmpty) None else Some(underlying.toList)
  }

  override def iterator: Iterator[(String, List[String])] =
    paramMap.keysIterator.map(s => s -> paramMap.getAll(s).toList)

  override def +[V1 >: List[String]](kv: (String, V1)): Map[String, V1] = new ParamMapView(paramMap + kv)
  override def -(key: String): Map[String, List[String]] = new ParamMapView(paramMap - key)
}
