package com.mikaelv.scalascrap.record

import org.scalatest.{Matchers, FunSpec}
import scala.reflect.runtime.universe._
import com.mikaelv.scalascrap.record
import scala.reflect.ClassTag
import scalaz.Monoid
import shapeless._

/**
 *
 */
class RecordSpec extends FunSpec with Matchers {
  case class Name(v: String)
  case class Age(v: Int)
  //case class Z(v: SampleEnum)

  // Provides a key using the Class name. Beware of ADT types: use Option(x) instead of Some(x)
  implicit def simpleNameRecordKeyProvider[A : TypeTag]: RecordKeyProvider[A] = RecordKeyProvider[A](RecordKey(implicitly[TypeTag[A]].tpe.toString))

  val name = Name("mikael")
  val name1 = Name("name1")
  val age = Age(25)
  val z = SampleEnum.Bar
  
  
  describe("A Record") {


    it("should allow to add new arbitrary fields")  {
      val e2 = Record(name).add(age)

      assert(typeOf[e2.type] <:< typeOf[Record[Name with Age]], "e2 conforms to Record[A with B]")
    }

    it("should allow to get an added field") {
      val e2 = Record(name).add(Age(10)).add(age)
      e2.get[Name] should be(name)
      e2.get[Age] should be(age)
    }

    it("should forbid to get a non added field") {
      val e2 = Record(name).add(age)
      assertTypeError("e2.get[Z]")
    }

    it("should allow to add optional fields") {
      val e2 = Record(Option(name))
      e2.get[Option[Name]] should be(Some(name))

      assertTypeError("e2.get[Name]")
    }

    it("should allow to write functions that accept a Record with more Fields than the type parameter") {
      def fn(record: Record[Name with Age]): Name = {
        record.get[Name]
      }

      fn(Record(name).add(age).add(z)) should be(name)
    }

  }

  describe("A RecordEncoder") {
    implicit val encoderMonoid = new Monoid[String] {

     override def append(e1: String, e2: => String): String =
        if (e1 == "") e2 else if (e2 == "") e1 else e1 + ", " + e2

      override def zero: String = ""
    }
    
    implicit val nameEncoder = new JsonEncoder[Name] {
      override def encode(t: Name): String = s"""name: "${t.v}""""
    }
    implicit val ageEncoder = new JsonEncoder[Age] {
      override def encode(t: Age): String = s"""age: ${t.v}"""
    }

    val rec = Record(name).add(Age(10))

    it("should encode a Record to Json") {
      val encoder = RecordEncoder[Name, String].add[Age]
      encoder.encode(rec) should be("""name: "mikael", age: 10""")
    }

    it("should be resolved implicitly") {
      type Person = Record[Name with Age]

      // Double transformation is identity
      implicit val personGeneric: Generic[Person] = RecordGeneric[Name].add[Age]
      val hl = personGeneric.to(rec)
      hl should be(Age(10) :: name :: HNil)
      personGeneric.from(hl) should be(rec)

      implicit object PersonGeneric extends Generic[Person] {
        override type Repr = Name :: Age :: HNil

        override def from(r: PersonGeneric.Repr): Person = Record(r.head).add(r.last)

        override def to(t: Person): PersonGeneric.Repr = t.get[Name] :: t.get[Age] :: HNil
      }
      import JsonEncoder._
      val encoder = JsonEncoder[Person] // TODO why it doesn't work if implicit object PersonGeneric is not defined ???
      encoder.encode(rec) should be("""name: "mikael", age: 10""")
    }
  }
}
