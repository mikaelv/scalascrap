package com.mikaelv.scalascrap.shapeless

import scala.annotation.implicitNotFound

/**
 * http://stackoverflow.com/questions/20311599/passing-a-shapeless-extensible-record-to-a-function-continued/23592210#23592210
 */
package object recordfunction {
  import shapeless._, ops.record.Selector, record._, syntax.singleton._

  val w1 = Witness("foo1")
  val w2 = Witness("foo2")
  val w3 = Witness("foo3")

  type HasFoo1[L <: HList] = Selector[L, w1.T] { type Out = String }
  type HasFoo2[L <: HList] = Selector[L, w2.T]
  type HasFoo3[L <: HList] = Selector[L, w3.T]

  @implicitNotFound("${L} should have foo1, foo2 and foo3")
  case class HasMyFields[L <: HList](implicit s1: HasFoo1[L], s2: HasFoo2[L], s3: HasFoo3[L])

  object HasMyFields {
    implicit def make[L <: HList : HasFoo1 : HasFoo2 : HasFoo3] = HasMyFields[L]
  }


  def fun1[L <: HList : HasMyFields](xs: L) = {
    val selectors = implicitly[HasMyFields[L]]
    import selectors._
    (xs("foo1").length, xs("foo2"), xs("foo3"))
  }

  fun1(("foo1"->> "hello") :: ("foo2" ->> 1)::("foo3" ->> 1.2)::HNil)

  // Does not compile: the value in foo1 is not a String
  //fun1(("foo1"->> 2)       :: ("foo2" ->> 1)::("foo3" ->> 1.2)::HNil)

}
