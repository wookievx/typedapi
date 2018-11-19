package typedapi.server.docs

import com.avsystem.commons.rest.openapi._
import typedapi.shared._

package object openapi extends TypeLevelFoldLeftLowPrio
                          with TypeLevelFoldLeftListLowPrio
                          with WitnessToStringLowPrio
                          with ApiTransformer {

  implicit val schemaResolver: SchemaResolver = new SchemaRegistry()

  implicit def singleStringSchema[T <: String]: RestSchema[T] = RestSchema.plain(Schema.String)

  def document = new DocumentOpenAPI

  def documentAll = new ComposeOpenAPI

}
