package typedapi.server.docs

import shapeless._
import shapeless.labelled.FieldType
import typedapi.shared._

import scala.annotation.implicitNotFound

@implicitNotFound("""Could not generate documentation for your api.

transformed: ${H}""")
trait DocComposition[H <: HList, DocRepr] {
  def docs: List[DocRepr]
}

object DocComposition extends DocCompositionLowPriority

trait DocCompositionLowPriority {
  implicit def hnilCompositionCase[DocRepr] = new DocComposition[HNil, DocRepr] {
    override def docs: List[DocRepr] = List.empty
  }

  implicit def defaultCompositionCase[
    El <: HList, KIn <: HList, VIn <: HList, MT <: MediaType, M <: MethodType, Out, Ds <: HList, DocRepr
  ](implicit
    derivation: DocDerivation[El, KIn, VIn, M, FieldType[MT, Out], DocRepr],
    next: DocComposition[Ds, DocRepr]
  ) = new DocComposition[(El, KIn, VIn, M, FieldType[MT, Out]) :: Ds, DocRepr] {
    override def docs: List[DocRepr] = derivation.derive :: next.docs
  }
}
