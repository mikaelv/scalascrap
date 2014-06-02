package com.mikaelv.scalascrap.record

/**
 * Created by mikael on 01/06/2014.
 */
object SampleDomain {

  class A
  case class B(value: String)
  case class C(value: String)
  case class D(value: String)
  class E
  class DD

  implicit val implicitA = new RecordKeyProvider[A](RecordKey("a"))
  implicit val implicitB = new RecordKeyProvider[B](RecordKey("b"))
  implicit val implicitC = new RecordKeyProvider[C](RecordKey("c"))
  implicit val implicitD = new RecordKeyProvider[D](RecordKey("d"))
  implicit val implicitE = new RecordKeyProvider[E](RecordKey("e"))
  implicit val implicitDD = new RecordKeyProvider[DD](RecordKey("dd"))



  type F1 = A with B
  type F2 = A with D
  type Enrich[X] = X with C with E
}
