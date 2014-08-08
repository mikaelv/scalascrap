package com.mikaelv.scalascrap.fpinscala

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}

/**
 * Created by mikael on 08/08/2014.
 */
object Chapter7Parallelism {
  // At the beginning, we considered Par to be a container of a value that we could simply get when it becomes available
  // Now it's a description of a parallel computation that gets interpreted at a later time by something like get.
  // It's more of a first-class program that we can run

  /*trait Par[A] {
  }*/

  type Par[A] = ExecutorService => Future[A]

  object Par {
    // primitive combinator
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
    private case class UnitFuture[A](get: A) extends Future[A] {
      override def cancel(p1: Boolean): Boolean = false

      override def isCancelled: Boolean = false

      override def get(p1: Long, p2: TimeUnit): A = get

      override def isDone: Boolean = true
    }

    // derived combinator
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    // Renamed to run def get[A](a: Par[A]) = ???
    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def map2[A, B , C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
      // Exercise 3 respect timeouts
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

    // fork musn't begin evaluating its argument immediately in parallel,
    // otherwise the implementation must know something about a Threadpool,
    // which means we wouldn't be able to use a different strategy for different parts of our program
    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        override def call(): A = a(es).get
      })
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size < 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

}
