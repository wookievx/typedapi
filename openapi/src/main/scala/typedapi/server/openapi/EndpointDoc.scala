package typedapi.server.openapi

import com.avsystem.commons.misc.OptArg
import com.avsystem.commons.rest.openapi._
import typedapi.shared.{MediaType => _, _}

sealed trait EndpointDoc[MT <: MethodType] {

  def prepend(segment: String): EndpointDoc[MT]
  def modifyOperation(f: Operation => Operation): EndpointDoc[MT]
  def modifyPath(f: PathItem => PathItem): EndpointDoc[MT]
  def addDefinition(schema: RefOr[Schema]): EndpointDoc[MT]
}

object EndpointDoc {

  private case class Full[MT <: MethodType](
    lens: PathItem => Operation,
    prism: PathItem => Operation => PathItem,
    definitions: Set[RefOr[Schema]] = Set.empty,
    pathString: OptArg[String] = OptArg.Empty,
    pathItem: PathItem
  ) extends EndpointDoc[MT] {
    override def prepend(segment: String): EndpointDoc[MT] = copy(
      pathString = s"$segment/${this.pathString}"
    )

    override def addDefinition(schema: RefOr[Schema]): EndpointDoc[MT] = copy(
      definitions = this.definitions + schema
    )

    override def modifyOperation(f: Operation => Operation): EndpointDoc[MT] = {
      copy(pathItem = prism(pathItem)(f(lens(pathItem))))
    }

    override def modifyPath(f: PathItem => PathItem): EndpointDoc[MT] = {
      copy(pathItem = f(pathItem))
    }
  }


  def getEmpty(responses: Responses): EndpointDoc[GetCall] = Full[GetCall](
    lens = _.get.get,
    prism = i => op => i.copy(get = op),
    pathItem = PathItem(get = Operation(responses = responses))
  )

  def postEmpty(responses: Responses): EndpointDoc[PostCall] = Full(
    lens = _.post.get,
    prism = i => op => i.copy(post = op),
    pathItem = PathItem(post = Operation(responses = responses))
  )

  def postBodyEmpty(responses: Responses): EndpointDoc[PostWithBodyCall] = Full(
    lens = _.post.get,
    prism = i => op => i.copy(post = op),
    pathItem = PathItem(post = Operation(responses = responses))
  )

  def putEmpty(responses: Responses): EndpointDoc[PutCall] = Full(
    lens = _.put.get,
    prism = i => op => i.copy(put = op),
    pathItem = PathItem(put = Operation(responses = responses))
  )

  def putBodyEmpty(responses: Responses): EndpointDoc[PutWithBodyCall] = Full(
    lens = _.put.get,
    prism = i => op => i.copy(put = op),
    pathItem = PathItem(put = Operation(responses = responses))
  )

  def deleteEmpty(responses: Responses): EndpointDoc[DeleteCall] = Full(
    lens = _.delete.get,
    prism = i => op => i.copy(delete = op),
    pathItem = PathItem(delete = Operation(responses = responses))
  )




}
