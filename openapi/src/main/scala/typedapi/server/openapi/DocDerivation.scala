package typedapi.server.openapi

import shapeless._
import shapeless.labelled.FieldType
import typedapi.shared._
import typedapi.shared.MediaType

import scala.annotation.implicitNotFound

@implicitNotFound(
  """Cannot find DocDerivation, Maybe Schema of a type cannot be found

elements: ${El}
input keys: ${KIn}
inout values: ${VIn}
method: ${M}
"""
)
trait DocDerivation[El <: HList, KIn <: HList, VIn <: HList, M <: MethodType, Resp, DocRepr] {
  def derive: DocRepr
}

object DocDerivation extends MidPriorityDerivations

trait LowPriorityDerivations {
  type Derivation[El <: HList, KIn <: HList, VIn <: HList, M <: MethodType, Resp, DocRepr] =
    DocDerivation[El, KIn, VIn, M, Resp, DocRepr]

  implicit def pathExtractor[S, El <: HList, KIn <: HList, VIn <: HList, M <: MethodType, Resp, DocRepr](implicit
    pathElem: PathElem[S, DocRepr],
    wit: Witness.Aux[S],
    show: WitnessToString[S],
    next: Derivation[El, KIn, VIn, M, Resp, DocRepr]
  ) =
    new Derivation[S :: El, KIn, VIn, M, Resp, DocRepr] {
      override def derive: DocRepr =
        pathElem.applyTo(wit.value, next.derive)
    }
}

trait MidPriorityDerivations extends LowPriorityDerivations {

  implicit def segmentExtractor[
    El <: HList, K, V, KIn <: HList, VIn <: HList, M <: MethodType, EIn <: HList, Resp, DocRepr
  ](implicit
    parameterSchema: ParameterSchema[K, V, SegmentInput, DocRepr],
    pathElem: PathElem[K, DocRepr],
    wit: Witness.Aux[K],
    show: WitnessToString[K],
    next: Derivation[El, KIn, VIn, M, Resp, DocRepr]
  ) = new Derivation[SegmentInput :: El, K :: KIn, V :: VIn, M, Resp, DocRepr] {
    override def derive: DocRepr =
      pathElem.applyTo(wit.value, parameterSchema.applyTo(wit.value, next.derive, ParameterSchema.Path))
  }

  implicit def queryExtractor[
    El <: HList, K, V, KIn <: HList, VIn <: HList, M <: MethodType, Resp, DocRepr
  ](implicit
    paramSchema: ParameterSchema[K, V, QueryInput, DocRepr],
    wit: Witness.Aux[K],
    next: Derivation[El, KIn, VIn, M, Resp, DocRepr]
  ) = new Derivation[QueryInput :: El, K :: KIn, V :: VIn, M, Resp, DocRepr] {
    override def derive: DocRepr = paramSchema.applyTo(wit.value, next.derive, ParameterSchema.Query)
  }

  implicit def queryOptExtractor[
    El <: HList, K, V, KIn <: HList, VIn <: HList, M <: MethodType, Resp, DocRepr
  ](implicit
    paramSchema: ParameterSchema[K, V, QueryInput, DocRepr],
    wit: Witness.Aux[K],
    next: Derivation[El, KIn, VIn, M, Resp, DocRepr]
  ) = new Derivation[QueryInput :: El, K :: KIn, Option[V] :: VIn, M, Resp, DocRepr] {
    override def derive: DocRepr = paramSchema.applyOpt(wit.value, next.derive, ParameterSchema.Query)
  }

  implicit def queryListExtractor[
    El <: HList, K, V, KIn <: HList, VIn <: HList, M <: MethodType, Resp, DocRepr
  ](implicit
    paramSchema: ParameterSchema[K, V, QueryInput, DocRepr],
    wit: Witness.Aux[K],
    next: Derivation[El, KIn, VIn, M, Resp, DocRepr]
  ) = new Derivation[QueryInput :: El, K :: KIn, List[V] :: VIn, M, Resp, DocRepr] {
    override def derive: DocRepr = paramSchema.applyMany(wit.value, next.derive, ParameterSchema.Query)
  }


  implicit def headerExtractor[
    El <: HList, K, V, KIn <: HList, VIn <: HList, M <: MethodType, Resp, DocRepr
  ](implicit
    paramSchema: ParameterSchema[K, V, HeaderInput, DocRepr],
    wit: Witness.Aux[K],
    next: Derivation[El, KIn, VIn, M, Resp, DocRepr]
  ) = new Derivation[HeaderInput :: El, K :: KIn, V :: VIn, M, Resp, DocRepr] {

    override def derive: DocRepr = paramSchema.applyTo(wit.value, next.derive, ParameterSchema.Header)
  }

  implicit def fixedHeaderExtractor[
    El <: HList, K, V, KIn <: HList, VIn <: HList, M <: MethodType, Resp, DocRepr
  ](implicit
    wit: Witness.Aux[K],
    next: Derivation[El, KIn, VIn, M, Resp, DocRepr]
  ) = new Derivation[FixedHeader[K, V] :: El, K :: KIn, V :: VIn, M, Resp, DocRepr] {
    override def derive: DocRepr = next.derive
  }

  implicit def headerSendExtractor[
    El <: HList, K, V, KIn <: HList, VIn <: HList, M <: MethodType, Resp, DocRepr
  ](implicit
    responseM: ResponseModifier[K, V, DocRepr],
    wit: Witness.Aux[K],
    next: Derivation[El, KIn, VIn, M, Resp, DocRepr]
  ) = new Derivation[ServerHeaderSend[K, V] :: El, KIn, VIn, M, Resp, DocRepr] {

    override def derive: DocRepr = responseM.applyTo(wit.value, next.derive)
  }

  implicit def headerMatchExtractor[
    El <: HList, K, V, KIn <: HList, VIn <: HList, M <: MethodType, Resp, DocRepr
  ](implicit
    wit: Witness.Aux[K],
    show: WitnessToString[K],
    parameterSchema: ParameterSchema[K, T, HeaderInput, DocRepr],
    next: Derivation[El, KIn, VIn, M, Resp, DocRepr]
  ) = new Derivation[ServerHeaderMatchInput :: El, K :: KIn, Map[String, V] :: VIn, M, Resp, DocRepr] {
    override def derive: DocRepr = parameterSchema.applyMany(wit.value, next.derive, ParameterSchema.Header)
  }

  implicit def getExtractor[MT <: MediaType, A, DocRepr](implicit
    responseSchema: ResponseOf[A, GetCall, MT, DocRepr],
  ) =
    new Derivation[HNil, HNil, HNil, GetCall, FieldType[MT, A], DocRepr] {
      override def derive: DocRepr = responseSchema.response
    }

  implicit def postExtractor[MT <: MediaType, A, DocRepr](implicit
    responseSchema: ResponseOf[A, PostCall, MT, DocRepr]
  ) =
    new Derivation[HNil, HNil, HNil, PostCall, FieldType[MT, A], DocRepr] {
      override def derive: DocRepr = responseSchema.response
    }

  implicit def postWithBodyExtractor[MT <: MediaType, BMT <: MediaType, Req, A, DocRepr](implicit
    responseSchema: ResponseOf[A, PostWithBodyCall, MT, DocRepr],
    requestSchema: BodySchema[Req, BMT, DocRepr]
  ) =
    new Derivation[HNil, FieldType[BMT, BodyField.T] :: HNil, Req :: HNil, PostWithBodyCall, FieldType[MT, A], DocRepr] {
      override def derive: DocRepr =
        requestSchema.applyTo(responseSchema.response)
    }

  implicit def putExtractor[MT <: MediaType, A, DocRepr](implicit
    responseSchema: ResponseOf[A, PutCall, MT, DocRepr]
  ) =
    new Derivation[HNil, HNil, HNil, PutCall, FieldType[MT, A], DocRepr] {
      override def derive: DocRepr = responseSchema.response
    }

  implicit def putWithBodyExtractor[MT <: MediaType, BMT <: MediaType, Req, A, DocRepr](implicit
    responseSchema: ResponseOf[A, PutWithBodyCall, MT, DocRepr],
    requestSchema: BodySchema[Req, BMT, DocRepr]
  ) =
    new Derivation[HNil, FieldType[BMT, BodyField.T] :: HNil, Req :: HNil, PutWithBodyCall, FieldType[MT, A], DocRepr] {
      override def derive: DocRepr =
        requestSchema.applyTo(responseSchema.response)
    }

  implicit def deleteExtractor[MT <: MediaType, A, DocRepr](implicit
    responseSchema: ResponseOf[A, DeleteCall, MT, DocRepr]
  ) =
    new Derivation[HNil, HNil, HNil, DeleteCall, FieldType[MT, A], DocRepr] {
      override def derive: DocRepr = responseSchema.response
    }
}