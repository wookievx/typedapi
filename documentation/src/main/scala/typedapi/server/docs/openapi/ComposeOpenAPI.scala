package typedapi.server.docs.openapi

import com.avsystem.commons.rest.openapi.OpenApi
import typedapi.server.FilterClientElementsList
import typedapi.server.docs.DocComposition
import typedapi.shared._
import shapeless._

class ComposeOpenAPI {

  def apply[H <: HList, FH <: HList, Fold <: HList](
    apiLists: CompositionCons[H]
  )(implicit
    filter: FilterClientElementsList.Aux[H, FH],
    folder: TypeLevelFoldLeftList.Aux[FH, Fold],
    composition: DocComposition[Fold, OpenAPIEndpointDoc]
  ): OpenApi = ???

}
