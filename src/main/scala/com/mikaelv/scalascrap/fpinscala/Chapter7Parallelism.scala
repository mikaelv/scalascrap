package com.mikaelv.scalascrap.fpinscala

import java.util.concurrent._
import scala.Some

/**
 * Created by mikael on 08/08/2014.
 */
object Chapter7Parallelism {
  // At the beginning, we considered Par to be a container of a value that we could simply get when it becomes available
  // Now it's a description of a parallel computation that gets interpreted at a later time by something like get.
  // It's more of a first-class program that we can run

  /*trait Par[A] {
  }*/

  type Par[A] = ExecutorService => TimedFuture[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(p1: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def get(p1: Long, p2: TimeUnit): A = get

    override def isDone: Boolean = true
  }

  case class TimedFuture[A](timeout: Long, future: Future[(A, Long)]) extends Future[(A, Long)] {

    override def cancel(p1: Boolean): Boolean = future.cancel(p1)

    override def isCancelled: Boolean = future.isCancelled

    override def get(): (A, Long) = {
      try {
        future.get(timeout, TimeUnit.MILLISECONDS)
      } catch {
        case e:TimeoutException =>
          val msg = s"${Thread.currentThread()} Exceeded timeout: $timeout when waiting for future completion"
          println(msg)
          throw new Exception(msg, e)
      }
    }

    override def get(p1: Long, p2: TimeUnit): (A, Long) = future.get(p1, p2)

    override def isDone: Boolean = future.isDone

  }

  object TimedFuture {
    def apply[A](f: => A): TimedFuture[A] = {
      val start = System.nanoTime()
      val a = f
      val duration = (System.nanoTime() - start) / 1000000
      TimedFuture(1000, UnitFuture(a, duration))
    }
  }



  object Par {
    // primitive combinator
    def unit[A](a: A): Par[A] = (es: ExecutorService) => TimedFuture { a }



    // Exercise 4
    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def parMap[A, B](l: Seq[A])(f: A => B): Par[Seq[B]] = {
      val fbs: Seq[Par[B]] = l.map(asyncF(f))
      sequence(fbs)
    }

    // Exercise 5
    def sequence[A](l: Seq[Par[A]]): Par[Seq[A]] = (es:ExecutorService) => {
      // Remark: The sequence is waiting in the main thread, which may not be desirable
      TimedFuture {
        l.foldLeft(Vector.empty[A]) { (vecA, pa) => pa(es).get._1 +: vecA }
      }

    }


    // derived combinator
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    // Renamed to run def get[A](a: Par[A]) = ???
    def run[A](s: ExecutorService)(a: Par[A]): Future[(A, Long)] = a(s)

    def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit(())){(a, _) => f(a)}

    def map2[A, B , C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
      // Exercise 3 respect timeouts
      // af and bf are executed concurrently
      TimedFuture {
        val af = a(es)
        val bf = b(es)
        val (ra, ta) = af.get()
        val (rb, tb) = bf.get(Math.max(0, bf.timeout -ta), TimeUnit.MILLISECONDS)
        f(ra, rb)
      }
    }

    // fork musn't begin evaluating its argument immediately in parallel,
    // otherwise the implementation must know something about a Threadpool,
    // which means we wouldn't be able to use a different strategy for different parts of our program
    def fork[A](a: => Par[A]): Par[A] =
      // Timeout set arbitrarily to 1s. For a good lib, it should be passed as an implicit parameter
      es => TimedFuture(1000, es.submit(new Callable[(A, Long)] {
        override def call(): (A, Long) = {
          val start = System.nanoTime()
          println(s"${Thread.currentThread()} Running forked computation")
          val res = a(es).get
          val duration = System.nanoTime() - start
          val result = (res._1, duration/1000000)
          println(s"${Thread.currentThread()} Ran forked computation. Result: $result")
          result
        }
      }))


    // TODO sum ends up in timeout if the thread pool size is < list size :-(
    def sum(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.size <= 1)
        Par.unit(ints.headOption getOrElse 0)
      else {
        val (l, r) = ints.splitAt(ints.length / 2)
        Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
      }


    def parFilter[A](l: List[A])(f: A => Boolean): Par[Seq[A]] = {
      val optA = l.map(asyncF { a =>
        if (f(a))
          Some(a)
        else
          None
      })
      Par.map(sequence(optA))(_.flatten)
    }
  }

  object ParTest {
    //import com.mikaelv.scalascrap.fpinscala.Chapter7Parallelism.Par
    //import java.util.concurrent.Executors

    val es = Executors.newFixedThreadPool(20)

    def f(x: Int) = { println(s"${Thread.currentThread()} - f($x)"); Thread.sleep(900); x * 2 }
    val p = Par.parMap(List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))( f )
    // Stackoverflow
    val p2 = Par.parMap(1 to 10000)( f )

    //val p = Par.fork(Par.unit(f(1)))
    Par.run(es)(p).get

    Par.run(es)(Par.sum(Vector(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))).get
  }


}
