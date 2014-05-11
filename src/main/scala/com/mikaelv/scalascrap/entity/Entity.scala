package com.mikaelv.scalascrap.entity

import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import com.mikaelv.scalascrap.entity.SampleDomain._
import scala.annotation.implicitNotFound

/**
 * Flexible entity implementation.
 * Shapeless calls this a record.
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
  type Enrich[X] = X with C with E
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

/** Implementation with clunky TypeTag and introspection */
class ImplPoly[F <: A] extends Intf {
  type R = Entity[F]
  type RR = Entity[F with C with E]

  def enrich(r: R)(implicit tagR: TypeTag[R], ev: R =:= F1): RR = {
    r.add[C](C("Default C")).add[E](new E)
  }

  def enrich(r: R)(implicit tagR: TypeTag[R]): RR = {
    val tpe = typeOf[R]
    //println(s"calling f2(r: $tpe)")
    val a = r.get[A]
    val rr = r.add(new E)
    //if (tpe <:< typeOf[Entity[F]])

    if (tpe =:= typeOf[Entity[F1]]) {
      val rF1 = r.asInstanceOf[Entity[F1]]
      val b = rF1.get[B].get
      rr.add[C](C("C for F1: "+ b))
    }
    else if (tpe =:= typeOf[Entity[F2]]) {
      val rF2 = r.asInstanceOf[Entity[F2]]
      val d = rF2.get[D].get
      rr.add[C](C("C for F2: " + d))
    }
    else
      rr.add[C](C("Default C"))


  }

  def write(r: RR) = { println (r) }


}



trait Sendable[R] {
  type RR <: R
  def enrich(r: Entity[R]): Entity[RR]
  def write(r: Entity[RR])

}

/** Implementation with beautiful type classes */
object Sendable {
  def send[R, RR](r: Entity[R])(implicit sendable: Sendable[R]) {
    val rr = sendable.enrich(r)
    sendable.write(rr)
  }

  abstract class SendableF[R <: A] extends Sendable[R] {


    def superenrich(r: Entity[R]) = {
      r.add[E](new E)
    }

    /* cannot implement:
    override def enrich(r: Entity[R]) = {
      r.add(new E)
    }
    This is because the return type will not be an RR.
    This is logical, as RR means all fields have been enriched
    */
    override def write(r: Entity[RR]) { println (r) }
  }

  implicit object SendableF1 extends SendableF[F1] {
    type RR = Enrich[F1]

    override def enrich(r: Entity[F1]): Entity[RR] = {
      val rr = superenrich(r)
      val b: B = r.get[B].get
      rr.add[C](C("C for F1: "+ b))
    }
  }

  implicit object SendableF2 extends SendableF[F2] {
    type RR = Enrich[F2]

    override def enrich(r: Entity[F2]): Entity[RR] = {
      val rr = superenrich(r)
      val d: D = r.get[D].get
      rr.add[C](C("C for F2: "+ d))
    }
  }


}


object EntityTest extends App {


  // Polymorphism: ImplPoly can process an Entity having an A, and if the Entity is a F1, can process the F1 fields

  val e1: Entity[F1] = Entity.empty.add(new A).add(B("B value"))
  val e2: Entity[F2] = Entity.empty.add(new A).add(D("D value"))
  val ea = Entity.empty.add(new A)
  val edd = Entity.empty.add(new A).add(D("D value")).add(new DD)

  new ImplPoly().send(e1)
  new ImplPoly().send(e2)
  new ImplPoly().send(ea) // should not compile
  new ImplPoly().send(edd) // should not compile
  // should not compile:
  //new ImplPoly().send(Entity.empty.add(new DD))


  println("\nType class implementation test")
  Sendable.send(e1)
  Sendable.send(e2)
  // does not compile :-) Sendable.send(ea)
  // does not compile :-) Sendable.send(edd)

}
