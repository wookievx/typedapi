package typedapi.server.docs

import typedapi.shared.{ApiOp, MediaType}
import ParameterSchema.Location

trait PathElem[K, Repr] {
  def applyTo(key: K, repr: Repr): Repr
}

trait BodySchema[T, MT <: MediaType, Repr] {
  def applyTo(mediaType: MT, repr: Repr): Repr
}

trait ParameterSchema[K, T, Loc <: ApiOp, Repr] {
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