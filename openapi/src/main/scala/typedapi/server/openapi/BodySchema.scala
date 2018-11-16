package typedapi.server.openapi

import typedapi.shared.MediaType
import ParameterSchema.Location

trait PathElem[K, Repr] {
  def applyTo(key: K, repr: Repr): Repr
}

trait Schema[K, T, Repr] {
  def applyTo(repr: Repr): Repr
}

trait BodySchema[T, MT <: MediaType, Repr] {
  def applyTo(repr: Repr): Repr
}

trait ParameterSchema[K, T, Repr] {
  def applyTo(key: K, repr: Repr, in: Location): Repr
  def applyOpt(key: K, repr: Repr, in: Location): Repr
  def applyMany(key: K, repr: Repr, in: Location): Repr
}

object ParameterSchema {

  sealed trait Location
  case object Header extends Location
  case object Path extends Location
  case object Query extends Location

}