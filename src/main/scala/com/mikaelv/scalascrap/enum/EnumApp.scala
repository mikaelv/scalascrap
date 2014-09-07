package com.mikaelv.scalascrap.enum

import com.mikaelv.scalascrap.enum.EnumApp.Side1.Side1

/**
 * Comparison of solutions for enumerations that are associated with a encoded String value.
 * For decoding from String to Enum, we want to associate unknown values to an "Other" enum type.
 */
object EnumApp extends App {
  trait Codec[T] {
    def encode(t: T): String
  }
  def encode[S : Codec](s: S): String = implicitly[Codec[S]].encode(s)

  // Enumeration
  object Side1 extends Enumeration {
    type Side1 = Val
    val Buy = new Val("1")
    val Sell = new Val("2")
    // we do not have to declare all values :-)
    // the default withName throws an exception if not found :-(
    def withName2(s: String): Value = values.find(_.toString == s).getOrElse(new Val(s))

    implicit val side1Codec = new Codec[Side1] {
      override def encode(t: Side1): String = t.toString()
    }
  }


  val s1: Side1 = Side1.Buy // each value is of type Side
  assert(s1.toString() == "1") // toString is not nice :-(
  assert(s1 == Side1.withName2("1")) // decoded value matches
  assert(encode(Side1.Buy) == "1") // type class discovery: we do not have to specify the superclass :-)
  assert("7" == (Side1.withName2("7") match {
    case Side1.Sell => "2" // no compiler warning :-(
    case otherValue => otherValue.toString
  }))


  // Case Objects
  sealed trait Side2 { def value: String }
  object Side2 {
    case object Buy extends Side2 { val value = "1"}
    case object Sell extends Side2 { val value = "2"}
    case class Other(value: String) extends Side2 // We can define a default enum :-)
    val values = List(Buy, Sell) // We have to declare all values :-(
    def withName(s: String): Side2 = values.find(_.value == s).getOrElse(Other(s))

    implicit val side2Codec = new Codec[Side2] {
      override def encode(t: Side2): String = t.value
    }
  }


  val s2: Side2 = Side2.Buy // each value is of type Side
  assert(s2.toString == "Buy") // toString is nice :-)
  assert(s2 == Side2.withName("1")) // decoded value matches
  assert(encode(Side2.Buy: Side2) == "1") // type class discovery: we have to specify the superclass :-(
  assert(encode(s2) == "1")               // but it's ok if assigned to a Side2 beforehand
  assert("7" == (Side2.withName("7") match {
    case Side2.Sell => "2" // compiler warning :-)
    case Side2.Other(value) => value
  }))

  // Case classes
  case class Side3(private val _value: String) { val value = _value}
  object Side3 {
    val Buy = Side3("1")
    val Sell = Side3("2")
    val values = List(Buy, Sell) // We have to declare all values :-(
    def withName(s: String): Side3 = values.find(_.value == s).getOrElse(Side3(s))

    implicit val side3Codec = new Codec[Side3] {
      override def encode(t: Side3): String = t.value
    }
  }


  val s3: Side3 = Side3.Buy // each value is of type Side
  assert(s3.toString == "Side3(1)")  // toString is ok, but not as nice as Side2
  assert(s3 == Side3.withName("1")) // decoded value matches
  assert(encode(Side3.Buy) == "1") // type class discovery: we do not have to specify the superclass :-)
  assert("7" == (Side3.withName("7") match {
    case Side3.Sell => "2" // no compiler warning :-(
    case otherValue => otherValue.value
  }))


}
