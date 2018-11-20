package typedapi.server.docs.openapi

import com.avsystem.commons.rest.openapi.{Server => _, _}
import org.specs2.mutable.Specification
import typedapi.dsl._
import DocDerivationSuite.RefOrUnwrapOps

class DocCompositionSuite extends Specification {

  case class Foo(name: String, surname: String)

  implicit val fooSchema: RestSchema[Foo] = RestStructure.materialize[Foo].standaloneSchema

  "doc composition builds correct OpenApi object" >> {
    val Api =
      (:= :> "foo" :> Segment[String]("name") :> Query[String]("surname"):> Put[Json, Foo]) :|:
      (:= :> "foo" :> Segment[String]("name") :> Get[Json, Foo]) :|:
      (:= :> "foo" :> "all" :> Query[Int]("limit") :> Post[Json, List[Foo]])

    val docs = documentAll(Api)

    "where all unique paths are present" >> {
      docs should beRight { d: OpenApi =>
        d.paths.paths.contains("/foo/{name}/") &&
        d.paths.paths.contains("/foo/all/")
      }
    }

    "where path items are merged" >> {
      docs should beRight { d: OpenApi =>
        val item = d.paths.paths("/foo/{name}/").assumeValue
        item.get.isDefined && item.put.isDefined
      }
    }
  }

}
