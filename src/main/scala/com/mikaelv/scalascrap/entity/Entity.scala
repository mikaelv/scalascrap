package com.mikaelv.scalascrap.entity

import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import com.mikaelv.scalascrap.entity.SampleDomain._

/**
 * Created by mikael on 10/05/2014.
 */
case class Entity[F](private val fields: Set[Any]) {
  def add[T](value: T): Entity[F with T] = new Entity[F with T](fields + value)
  def get[T](implicit tag: ClassTag[T], ev: F <:< T): Option[T] = fields collectFirst {case t: T => t}


}

object Entity {
  def empty: Entity[Unit] = new Entity(Set.empty)
}



object SampleDomain {

  class A
  case class B(value: String)
  case class C(value: String)
  case class D(value: String)
  class E
  class DD

  type F1 = A with B with Unit
  type F2 = A with D with Unit
}


abstract class Intf {
  type R
  type RR

  def send(r: R)(implicit tagR: TypeTag[R]) {
    val rr = enrich(r)
    write(rr)
  }

  def enrich(r: R)(implicit tagR: TypeTag[R]): RR
  def write(r: RR)
}

class ImplPoly[F <: A] extends Intf {
  type R = Entity[F]
  type RR = Entity[F with C with E]

  def enrich(r: R)(implicit tagR: TypeTag[R]): RR = {
    val tpe = typeOf[R]
    println(s"calling f2(r: $tpe)")

    val rr = r.add(new E)
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
      rr.add[C](C("Default C"))


  }

  def write(r: RR) = { println (r) }


}



object EntityTest extends App {


  // Polymorphism

  val e1: Entity[F1] = Entity.empty.add(new A).add(B("B value"))
  new ImplPoly[F1]().send(e1)

  new ImplPoly().send(Entity.empty.add(new A).add(D("D value")))

  new ImplPoly().send(Entity.empty.add(new A))
  // should not compile:
  //new ImplPoly().f2(new Entity[DD]())
  new ImplPoly().send(Entity.empty.add(new A).add(D("D value")).add(new DD)) // should not compile
}
