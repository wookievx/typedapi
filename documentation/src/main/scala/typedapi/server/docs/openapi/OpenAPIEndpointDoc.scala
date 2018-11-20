package typedapi.server.docs.openapi

import com.avsystem.commons.rest.openapi._
import typedapi.server.docs._
import typedapi.shared.{MediaType => _, _}
import OpenAPIEndpointDoc.MethodLens
import typedapi.server.StatusCodes

case class OpenAPIEndpointDoc(
  lens: MethodLens[_ <: MethodType],
  definitions: Set[RefOr[Schema]] = Set.empty,
  pathString: String = "/",
  item: PathItem
) {

  def prepend(segment: String): OpenAPIEndpointDoc = copy(
    pathString = s"/$segment${this.pathString}"
  )

  def modifyOperation(f: Operation => Operation): OpenAPIEndpointDoc = copy(
    item = lens.modify(item)(f(lens.get(item)))
  )

  def modifyItem(f: PathItem => PathItem): OpenAPIEndpointDoc = copy(
    item = f(item)
  )

  def addDefinition(schema: RefOr[Schema]): OpenAPIEndpointDoc = copy(
    definitions = definitions + schema
  )
}

object OpenAPIEndpointDoc {

  case class MethodLens[M <: MethodType](get: PathItem => Operation, modify: PathItem => Operation => PathItem) {
    def forType[NM <: MethodType]: MethodLens[NM] = copy[NM]()
  }

  implicit val getLens: MethodLens[GetCall] = MethodLens(
    _.get.get,
    pi => op => pi.copy(get = op)
  )

  implicit val postLens: MethodLens[PostCall] = MethodLens(
    _.post.get,
    pi => op => pi.copy(post = op)
  )

  implicit val postBodyLens: MethodLens[PostWithBodyCall] = postLens.forType

  implicit val putLens: MethodLens[PutCall] = MethodLens(
    _.put.get,
    pi => op => pi.copy(put = op)
  )

  implicit val putBodyLens: MethodLens[PutWithBodyCall] = putLens.forType

  implicit val deleteLens: MethodLens[DeleteCall] = MethodLens(
    _.delete.get,
    pi => op => pi.copy(delete = op)
  )

  import typedapi.shared.MediaType
  private class OpenApiEndpointResponse[T, M <: MethodType, MT <: MediaType](
    lens: MethodLens[M],
    resolver: SchemaResolver,
    restSchema: RestSchema[T]
  ) extends ResponseOf[T, M, MT, OpenAPIEndpointDoc] {
    override def response(mediaType: MT): OpenAPIEndpointDoc = {
      val schema = resolver.resolve(restSchema)
      val responses = Responses(
        Map(
          StatusCodes.Ok.statusCode ->
          RefOr(
            Response(
              content = Map(
                mediaType.value ->
                MediaType(
                  schema = schema
                )
              )
            )
          )
        )
      )

      val item =
      lens.modify(
        PathItem()
      )(
        Operation(
          responses = responses
        )
      )

      OpenAPIEndpointDoc(
        lens = lens,
        definitions = Set(schema),
        item = item
      )
    }
  }


  implicit def responseOf[T, M <: MethodType, MT <: MediaType](implicit
    lens: MethodLens[M],
    resolver: SchemaResolver,
    restSchema: RestSchema[T]
  ): ResponseOf[T, M, MT, OpenAPIEndpointDoc] = new OpenApiEndpointResponse(lens, resolver, restSchema)

  private class HeaderResponseModifier[K, T](
    resolver: SchemaResolver,
    restSchema: RestSchema[T]
  ) extends ResponseModifier[K, T, OpenAPIEndpointDoc] {
    override def applyTo(key: K, repr: OpenAPIEndpointDoc): OpenAPIEndpointDoc = {
      val schema = resolver.resolve(restSchema)
      val header = Header(
        schema = schema
      )
      repr modifyOperation { op =>
        val responses = op.responses.copy(
          byStatusCode = op.responses.byStatusCode.mapValues({
            case RefOr.Value(v) => RefOr(v.copy(headers = v.headers + (key.toString -> RefOr(header))))
            case ref => ref
          })
        )
        op.copy(
          responses = responses
        )
      }
    }
  }

  implicit def headerModifier[K, T](implicit
    resolver: SchemaResolver,
    restSchema: RestSchema[T]
  ): ResponseModifier[K, T, OpenAPIEndpointDoc] = new HeaderResponseModifier(resolver, restSchema)


  private class BodyEndpointModifier[T, MT <: MediaType](
    resolver: SchemaResolver,
    restSchema: RestSchema[T]
  ) extends BodySchema[T, MT, OpenAPIEndpointDoc] {
    override def applyTo(mediaType: MT, repr: OpenAPIEndpointDoc): OpenAPIEndpointDoc = {
      val schema = resolver.resolve(restSchema)
      val reqBody = RequestBody(
        content = Map(
          mediaType.value ->
          MediaType(
            schema = schema
          )
        ),
        required = true
      )
      repr modifyOperation { op =>
        op.copy(
          requestBody = RefOr(reqBody)
        )
      }
    }
  }

  implicit def bodyEndpointModifier[T, MT <: MediaType](implicit
    resolver: SchemaResolver,
    restSchema: RestSchema[T]
  ): BodySchema[T, MT, OpenAPIEndpointDoc] = new BodyEndpointModifier(resolver, restSchema)

  sealed trait LocationStrategy[Loc] {
    def modelLocation: Location
    def many(parameter: Parameter): Parameter
  }

  implicit case object HeaderStrategy extends LocationStrategy[HeaderInput] {
    override def modelLocation: Location = Location.Header
    override def many(parameter: Parameter): Parameter = parameter.copy(
      description =
        s"Any header with name starting with: ${parameter.name} which is: ${parameter.description.getOrElse("")}",
      required = false
    )
  }

  implicit case object QueryStrategy extends LocationStrategy[QueryInput] {
    override def modelLocation: Location = Location.Query
    override def many(parameter: Parameter): Parameter = {
      parameter.copy(
        schema =
          RefOr(Schema.arrayOf(parameter.schema.get, uniqueItems = false))
      )
    }
  }

  implicit case object SegmentStrategy extends LocationStrategy[SegmentInput] {
    override def modelLocation: Location = Location.Path
    override def many(parameter: Parameter): Parameter = parameter
  }

  private class ParameterEndpointModifier[K, T, Loc <: ApiOp](
    resolver: SchemaResolver,
    restSchema: RestSchema[T],
    locationStrategy: LocationStrategy[Loc]
  ) extends ParameterSchema[K, T, Loc, OpenAPIEndpointDoc] {

    override def applyTo(key: K, repr: OpenAPIEndpointDoc, in: ParameterSchema.Location): OpenAPIEndpointDoc = {
      val param = paramOf(key)
      addParam(repr, param)
    }

    override def applyOpt(key: K, repr: OpenAPIEndpointDoc, in: ParameterSchema.Location): OpenAPIEndpointDoc = {
      val param = paramOf(key).copy(required = false)
      addParam(repr, param)
    }

    override def applyMany(key: K, repr: OpenAPIEndpointDoc, in: ParameterSchema.Location): OpenAPIEndpointDoc = {
      val param = locationStrategy.many(paramOf(key))
      addParam(repr, param)
    }

    private def paramOf(key: K) = {
      Parameter(
        name = key.toString,
        in = locationStrategy.modelLocation,
        required = true,
        schema = resolver.resolve(restSchema)
      )
    }

    private def addParam(repr: OpenAPIEndpointDoc, param: Parameter) = {
      repr modifyOperation { op =>
        op.copy(
          parameters = RefOr(param) :: op.parameters
        )
      }
    }
  }

  implicit def parameterEndpointModifier[K, T, Loc <: ApiOp](implicit
    resolver: SchemaResolver,
    restSchema: RestSchema[T],
    locationStrategy: LocationStrategy[Loc]
  ): ParameterSchema[K, T, Loc, OpenAPIEndpointDoc] =
    new ParameterEndpointModifier(resolver, restSchema, locationStrategy)


  implicit def pathElem[K]: PathElem[K, OpenAPIEndpointDoc] = (k, repr, isQuery) => {
    if (isQuery) repr.prepend(s"{$k}")
    else repr.prepend(k.toString)
  }

}
