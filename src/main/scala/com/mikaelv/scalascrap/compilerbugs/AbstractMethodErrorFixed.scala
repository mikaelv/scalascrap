package com.mikaelv.scalascrap.compilerbugs

/**
 * Fixes the issue discovered in [[AbstractMethodError]]
 */
object AbstractMethodErrorFixed extends App {

  class E[F]
  class A
  class B
  class C
  class D
  type F1 = A with B
  type F2 = F1 with C with D



  // solution: use type parameters instead of abstract types
  trait Intf[R, RR] {

    def f1(r: R) {
      val rr = f2(r)
      f3(rr)
    }

    def f2(r: R): RR
    def f3(r: RR)
  }

  class Impl extends Intf[E[F1], E[F2]] {

    // works if I replace by:
    //type RR = E[A with B with C]

    override def f2(r: E[F1]): E[F2] = new E[F1 with C with D]()
    override def f3(rr: E[F2]) { println("This should be printed")}

  }


  new Impl().f1(new E[A with B]())




}
