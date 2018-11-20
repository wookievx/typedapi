package typedapi.server.docs.openapi

import com.avsystem.commons._
import com.avsystem.commons.rest.openapi._
import typedapi.server.FilterClientElementsList
import typedapi.server.docs.DocComposition
import typedapi.shared._
import shapeless._
import ComposeOpenAPI._

/**
  * Class responsible for combining documentation of multiple endpoints
  * Note: For now it is assumed that no two endpoints contains the same path and method,
  * because merge in this situation is very difficult. So the following code will fail at runtime:
  * {{{
  *   val Api = (:= :> "path" :> Get[Json, MyType]) :|:
  *             (:= :> "path" :> Header[String]("header") :> Get[Json, MyType])
  *
  *   val docs = documentAll(Api) // throw exception
  * }}}
  *
  */
class ComposeOpenAPI {

  def apply[H <: HList, FH <: HList, Fold <: HList](
    apiLists: CompositionCons[H],
    info: Info = Info("Endpoints", "1.0.0")
  )(implicit
    filter: FilterClientElementsList.Aux[H, FH],
    folder: TypeLevelFoldLeftList.Aux[FH, Fold],
    composition: DocComposition[Fold, OpenAPIEndpointDoc]
  ): Either[Exception, OpenApi] = {
    val singular = composition.docs
    (singular foldLeft OpenApi(info = info, paths = Paths(Map.empty)).asRight[Exception]) { (docRes, derived) =>
      docRes.fold(
        _.asLeft,
        doc => {
          val path = derived.pathString

          val oldItem = doc.paths.paths.get(path).map({
            case RefOr.Value(v) => Right(v)
            case ref => Left(new IllegalStateException(s"Encountered item is reference: $ref"))
          })

          val itemResult = oldItem.map(_.flatMap(mergeItems(_, derived.item))).getOrElse(Right(derived.item))

          itemResult map { i =>
            doc.copy(
              paths = Paths(doc.paths.paths.updated(path, RefOr(i)))
            )
          }
        }
      )
    }
  }


}


object ComposeOpenAPI {

  private[openapi] implicit class EitherOps[V](private val value: V) extends AnyVal {
    def asRight[L]: Either[L, V] = Right(value)
    def asLeft[R]: Either[V, R] = Left(value)
  }

  case class MergeError(
    method: String,
    leftOp: Operation,
    rightOp: Operation
  ) extends Exception(s"Conflicting operations for method $method: $leftOp, $rightOp")

  private[openapi] implicit class FunctionOps[A, L, R](private val f: A => Either[L, R]) extends AnyVal {

    def andThenEither[RL >: L, RR](g: R => Either[RL, RR]): A => Either[RL, RR] = { a => f(a).flatMap(g) }

  }

  private def mergeItems(
    lhs: PathItem,
    rhs: PathItem
  ): Either[MergeError, PathItem] = {

    def mergeAt(method: String,
      access: PathItem => OptArg[Operation],
      modify: PathItem => OptArg[Operation] => PathItem
    )(
      item: PathItem
    ): Either[MergeError, PathItem] = {
      val leftOpt = access(lhs).toOption
      val rightOpt = access(rhs).toOption
      val combined = leftOpt.flatMap(l => rightOpt.map(r => Left(MergeError(method, l, r))))
      val op = combined.getOrElse(Right(leftOpt.orElse(rightOpt).toOptArg))
      op.map(modify(item))
    }

    def mergeSchemas: PathItem = {
      val params = lhs.parameters ++ rhs.parameters

      val summary =
        lhs.summary.toOption
          .flatMap(l => rhs.summary.toOption.map(l.concat))
          .orElse(lhs.summary.toOption)
          .orElse(rhs.summary.toOption)

      val description =
        lhs.description.toOption
          .flatMap(l => rhs.description.toOption.map(l.concat))
          .orElse(lhs.description.toOption)
          .orElse(rhs.description.toOption)

      val servs = lhs.servers ++ rhs.servers

      lhs.copy(
        summary = summary.toOptArg,
        description = description.toOptArg,
        servers = servs,
        parameters = params
      )
    }


    val func =
    { mergeAt("get", _.get, i => op => i.copy(get = op)) _ } andThenEither
      mergeAt("post", _.post, i => op => i.copy(post = op)) andThenEither
      mergeAt("put", _.put, i => op => i.copy(put = op)) andThenEither
      mergeAt("delete", _.delete, i => op => i.copy(delete = op)) andThenEither
      mergeAt("patch", _.patch, i => op => i.copy(patch = op)) andThenEither
      mergeAt("options", _.options, i => op => i.copy(options = op)) andThenEither
      mergeAt("head", _.head, i => op => i.copy(head = op))

    func(mergeSchemas)
  }


}