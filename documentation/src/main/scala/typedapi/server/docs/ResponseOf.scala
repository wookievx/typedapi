package typedapi.server.docs

import typedapi.shared.{MediaType, MethodType}

trait ResponseOf[V, M <: MethodType, MT <: MediaType, R] {
  def response(mediaType: MT): R
}


trait ResponseModifier[K, V, Repr] {
  def applyTo(key: K, repr: Repr): Repr
}
