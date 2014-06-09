package com.mikaelv.scalascrap.record

import org.scalatest.{Matchers, FunSpec}
import scala.reflect.runtime.universe._

/**
 *
 */
class RecordSpec extends FunSpec with Matchers {
  case class X(v: String)
  case class Y(v: Int)
  //case class Z(v: SampleEnum)

  implicit val implicitX = RecordKeyProvider[X](RecordKey("x"))
  implicit val implicitY = RecordKeyProvider[Y](RecordKey("y"))
  implicit val implicitZ = RecordKeyProvider[SampleEnum](RecordKey("sampleEnum"))
  
  val x = X("x")
  val x1 = X("x1")
  val y = Y(25)
  val z = SampleEnum.Bar
  
  
  describe("A Record") {


    it("should allow to add new arbitrary fields")  {
      val e2 = Record(x).add(y)

      assert(typeOf[e2.type] <:< typeOf[Record[X with Y]], "e2 conforms to Record[A with B]")
    }

    it("should allow to get an added field") {
      val e2 = Record(x).add(Y(10)).add(y)
      e2.get[X] should be(x)
      e2.get[Y] should be(y)
    }

    it("should forbid to get a non added field") {
      val e2 = Record(x).add(y)
      assertTypeError("e2.get[Z]")
    }

    it("should allow to add optional fields") {
      val e2 = Record(Some(x))
      e2.get[Option[X]] should be(Some(x))

      assertTypeError("e2.get[X]")
    }

    it("should allow to write functions that accept a Record with more Fields than the type parameter") {
      def fn(record: Record[X with Y]): X = {
        record.get[X]
      }

      fn(Record(x).add(y).add(z)) should be(x)
    }

  }

  describe("A RecordEncoder") {
    implicit val encoderMonoid = new EncoderMonoid[String] {
      override def op(e1: String, e2: String): String = e1 + "\n" + e2

      override def zero: String = ""
    }
    implicit val xEncoder = new Encoder[X, String] {
      override def encode(t: X): String = "X="+x.v
    }
    implicit val yEncoder = new Encoder[Y, String] {
      override def encode(t: Y): String = "Y="+y.v
    }
    implicit val sampleEnumEncoder = new Encoder[SampleEnum, String] {
      override def encode(t: SampleEnum): String = "SampleEnum="+t.name()
    }
    implicit val nilEncoder = new Encoder[Unit, String] {
      override def encode(t: Unit): String = ""
    }
    implicit val nilKeyProvider = RecordKeyProvider[Unit](RecordKey("nil"))


    //implicit val re0 = RecordEncoder[Unit, String]
    //implicit val re1 = RecordEncoder[X with Unit, String]
    //val encoder = RecordEncoder[X with Y, String].add[SampleEnum]
    val encoder = RecordEncoder[X, String].add[Y].add[SampleEnum]

    it("should encode a Record to a String") {
      val rec = Record(x).add(Y(10)).add(z)
      encoder.encode(rec) should be(
        """
          |X=x
          |Y=25
          |SampleEnum=Bar""".stripMargin)
    }

    /* TODO it("should be resolved implicitly") {
      val rec = Record(x).add(Y(10)).add(z)
      rec.encode should be(
        """
          |X=x
          |Y=25
          |SampleEnum=Bar""".stripMargin)
    }*/
  }
}
