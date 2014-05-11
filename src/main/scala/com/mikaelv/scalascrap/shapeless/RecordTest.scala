package com.mikaelv.scalascrap.shapeless

import shapeless._
import shapeless.ops.record.{Selector, Keys}
import syntax.singleton._
import scala.annotation.implicitNotFound
import record._

/**
 * Created by mikael on 10/05/2014.
 */
object RecordTest extends App {

  abstract class Intf {



    def send[R <: HList : HasOrderFields, F](r: R) {
      //import HasOrderFields.make
      //val a = r("a")
      val rr = enrich(r)
      write(rr)
    }

    def enrich[R <: HList : HasOrderFields, RR <: HList](r: R): RR

    def write[RR](r: RR)
  }

  class ImplPoly extends Intf {

    override def enrich[R <: HList : HasOrderFields, RR](r: R): RR = {


      /*val rr = r.add(new E)
      // TODO add a common enrichment
      //if (tpe <:< typeOf[Entity[F]])

      if (tpe =:= typeOf[Entity[F1]]) {
        val rF1 = r.asInstanceOf[Entity[F1]]
        val b = rF1.get[B].get
        rr.add[C](C("Specialized C for F1: "+ b))
      }
      else if (tpe =:= typeOf[Entity[F2]]) {
        val rF2 = r.asInstanceOf[Entity[F2]]
        val d = rF2.get[D].get
        rr.add[C](C("Specialized C for F2: " + d))
      }
      else
        rr.add[C](C("Default C"))*/

      ???
    }

    def write[RR](r: RR) = {
      println(r)
    }


  }


  class A
  case class B(value: String)
  case class C(value: String)
  case class D(value: String)
  class E
  class DD

  val wa = Witness("a")
  val we = Witness("e")

  type HasA[L <: HList] = Selector[L, wa.T]
  type HasE[L <: HList] = Selector[L, we.T]

  @implicitNotFound("${L} does not have fields a and e")
  case class HasOrderFields[L <: HList](implicit s1: HasA[L], s2: HasE[L])
  object HasOrderFields {
    implicit def make[L <: HList : HasA : HasE] = new HasOrderFields[L]
  }


  //@implicitNotFound("${L} does not have fields ${F1} and ${F2}")
  class Has2Fields[L <: HList, F1 <: Witness#T, F2 <: Witness#T]



  class Has2FieldsImpl[F1 <: Witness#T, F2 <: Witness#T] {
    type HasF1[L <: HList] = Selector[L, F1]
    type HasF2[L <: HList] = Selector[L, F2]
    implicit def make[L <: HList : HasF1 : HasF2] = new Has2Fields[L, F1 , F2 ]
  }

  /*class HasOrderFields2[L <: HList] extends Has2Fields[L, wa.T, we.T] {

  }*/
  class HasOrderFields2[L <: HList] extends Has2Fields[L, wa.T, we.T]
  object HasOrderFields2 extends Has2FieldsImpl[wa.T, we.T] {
    implicit override def make[L <: HList : HasF1 : HasF2] = new HasOrderFields2[L]
  }

  //type HasOrderFields2[L <: HList] = Has2Fields[L, wa.T, we.T ]

/*    type HasF1[L1 <: HList] = Selector[L1, f1.T]
    type HasF2[L1 <: HList] = Selector[L1, f2.T]
    implicit def make[L <: HList : HasF1 : HasF2 ] = new Has2Fields[L, f1.type, f2.type](f1, f2)*/


  //class HasOrderFields2[L <: HList]
  /*type HasOrderFields2[L <: HList] = Has2Fields[L, wa.type, we.type ]
  object HasOrderFields2 extends Has2Fields(wa, we) {
    override implicit def make[L <: HList : HasF1 : HasF2 ] = new Has2Fields[L, f1.type, f2.type](f1, f2)
  }*/
  /*object HasOrderFields2 extends Has2Fields(wa, we) {
    implicit def make2[L <: HList : HasF1 : HasF2 ] = new HasOrderFields2[L]
    //override implicit def make[L <: HList : HasF1 : HasF2 ] = new HasOrderFields2()
  }*/



  val order1 = ("a" ->> new A) :: ("b" ->> B("B")) :: ("e" ->> new E) :: HNil
  val a = order1("a")
  val order2 = ("a" ->> new A) :: ("d" ->> D("D")) :: HNil

  import HasOrderFields2._
  new ImplPoly().send(order1)

  //new ImplPoly().send(order2)
  // should not compile:
  //new ImplPoly().send( ("dd" ->> new DD) :: HNil)




    // Polymorphism

    /*val e1: Entity[F1] = Entity.empty.add(new A).add(B("B value"))
    new ImplPoly[F1]().send(e1)

    new ImplPoly().send(Entity.empty.add(new A).add(D("D value")))

    new ImplPoly().send(Entity.empty.add(new A))
    // should not compile:
    //new ImplPoly().f2(new Entity[DD]())
    new ImplPoly().send(Entity.empty.add(new A).add(D("D value")).add(new DD)) // should not compile
  }*/



}
