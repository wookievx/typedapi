package typedapi.server.openapi

import typedapi.shared.{MediaType, MethodType}

trait ResponseOf[V, M <: MethodType, MT <: MediaType, R] {
  def response: R
}


trait ResponseModifier[K, V, Repr] {
  def applyTo(key: K, repr: Repr): Repr
}
