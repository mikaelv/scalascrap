package com.mikaelv.scalascrap.compilerbugs

/**
 * [[https://issues.scala-lang.org/browse/SI-8575]]
 */
object AbstractMethodError extends App {

  class E[F]
  class A
  class B
  class C
  class D
  type F1 = A with B
  type F2 = F1 with C with D


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

    type R = E[F1]
    type RR = E[F1 with C]
    // works if I replace by:
    //type RR = E[A with B with C]

    override def f2(r: E[F1]): E[F1 with C] = new E[F1 with C]()
    override def f3(rr: E[F1 with C]) { println("This should be printed")}

  }


  new Impl().f1(new E[A with B]())


}
