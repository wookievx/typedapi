package typedapi.server.docs.openapi

import com.avsystem.commons.rest.openapi.{MediaType => _, _}
import shapeless._
import shapeless.labelled.FieldType
import typedapi.server.FilterClientElements
import typedapi.server.docs.DocDerivation
import typedapi.shared._

class DocumentOpenAPI {

  def apply[H <: HList, FH <: HList, El <: HList, KIn <: HList, VIn <: HList, M <: MethodType, MT <: MediaType, Out](
    apiList: ApiTypeCarrier[H],
    info: Info = Info(title = "Endpoint", version = "1.0.0")
  )(implicit
    filter: FilterClientElements.Aux[H, FH],
    folder: Lazy[TypeLevelFoldLeft.Aux[FH, Unit, (El, KIn, VIn, M, FieldType[MT, Out])]],
    docsDerivation: DocDerivation[El, KIn, VIn, M, FieldType[MT, Out], OpenAPIEndpointDoc]
  ): OpenApi = {
    val derivation = docsDerivation.derive
    OpenApi(
      openapi = OpenApi.Version,
      info = info,
      paths = Paths(
        Map(
          derivation.pathString ->
          RefOr(derivation.item)
        )
      )
    )
  }



}
