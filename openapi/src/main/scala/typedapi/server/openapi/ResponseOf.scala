package typedapi.server.openapi

import typedapi.shared.MediaType

trait ResponseOf[V, MT <: MediaType, R] {
  def response: R
}

object ResponseOf {

}

trait ResponseModifier[V, Repr] {
  def applyTo(repr: Repr): Repr
}
