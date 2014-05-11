package com.mikaelv.scalascrap.compilerbugs

/**
 * Created by mikael on 10/05/2014.
 */
object AbstractMethodError extends App {

  class E[F]
  class A
  class B
  class C
  class D



  trait Intf {
    type R
    type RR

    def f1(r: R) {
      val rr = f2(r)
      f3(rr)
    }

    def f2(r: R): RR
    def f3(r: RR)
  }

  class Impl extends Intf {

    type F = A with B
    type R = E[F]
    type RR = E[F with C]
    // works if I replace by:
    //type RR = E[A with B with C]

    override def f2(r: R): RR = new E[F with C]()
    override def f3(rr: RR) { println("This should be printed")}

  }



  // https://issues.scala-lang.org/browse/SI-8575
  new Impl().f1(new E[A with B]())




}
