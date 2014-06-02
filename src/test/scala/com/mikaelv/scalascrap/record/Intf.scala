package com.mikaelv.scalascrap.record


import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import scala.annotation.implicitNotFound
import com.mikaelv.scalascrap.record.SampleDomain._
import com.mikaelv.scalascrap.record.SampleDomain.B
import com.mikaelv.scalascrap.record.SampleDomain.C

/**
 * Attempt to use polymorphisms with Records
 */
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
  type R = Record[F]
  type RR = Record[F with C with E]



  def enrich(r: R)(implicit tagR: TypeTag[R], ev: R =:= F1): RR = {
    r.add[C](C("Default C")).add[E](new E)
  }

  def enrich(r: R)(implicit tagR: TypeTag[R]): RR = {
    val tpe = typeOf[R]
    //println(s"calling f2(r: $tpe)")
    val a = r.get[A]
    val rr = r.add(new E)
    //if (tpe <:< typeOf[Entity[F]])

    if (tpe =:= typeOf[Record[F1]]) {
      val rF1 = r.asInstanceOf[Record[F1]]
      val b = rF1.get[B]
      rr.add[C](C("C for F1: "+ b))
    }
    else if (tpe =:= typeOf[Record[F2]]) {
      val rF2 = r.asInstanceOf[Record[F2]]
      val d = rF2.get[D]
      rr.add[C](C("C for F2: " + d))
    }
    else
      rr.add[C](C("Default C"))


  }

  def write(r: RR) = { println (r) }


}
