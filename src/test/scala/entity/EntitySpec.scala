package entity

import org.scalatest.{Matchers, FunSpec}
import scala.reflect.runtime.universe._

/**
 *
 */
class EntitySpec extends FunSpec with Matchers {
  case class X(v: String)
  case class Y(v: Int)
  case class Z(v: String)
  
  val x = X("x")
  val x1 = X("x1")
  val y = Y(25)
  
  
  describe("An Entity") {
    val e = Entity.empty

    it("should allow to add new arbitrary fields")  {
      val e2 = e.add(x).add(y)
      //type T = e2.type

      assert(typeOf[e2.type] <:< typeOf[Entity[Unit with X with Y]], "e2 conforms to Entity[Unit with A with B]")
    }

    it("should allow to get an added field") {
      val e2 = e.add(x).add(y)
      e2.get[X] should be(Some(x))
      e2.get[Y] should be(Some(y))
    }

    it("should forbid to get a non added field") {
      val e2 = e.add(x).add(y)
      assertTypeError("e2.get[Z]")
    }

    it("should forbid to add the same field type twice") {
      // TODO assertTypeError("""e.add(x).add(x1)""")
    }


  }
}
