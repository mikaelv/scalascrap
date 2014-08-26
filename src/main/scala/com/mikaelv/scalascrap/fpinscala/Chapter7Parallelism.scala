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

    def parMap[A, B](l: Seq[A])(f: A => B): Par[IndexedSeq[B]] = {
      val fbs: Seq[Par[B]] = l.map(asyncF(f))
      sequence(fbs)
    }

    // Exercise 5
    def sequence[A](l: Seq[Par[A]]): Par[IndexedSeq[A]] = (es:ExecutorService) => {
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

    def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = flatten(map(a)(f))

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

    def map3[A, B , C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
      val cd: Par[C => D] = map2(a, b)(f.curried(_)(_))
      map2(cd, c)(_(_))
    }

    def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = {
      val ab: Par[(A, B)] = map2(a, b)((a,b) => (a, b))
      val cd: Par[(C, D)] = map2(c, d)((c, d) => (c, d))
      val solution1 = map2(ab, cd)((ab, cd) => f(ab._1, ab._2, cd._1, cd._2))

      val cde: Par[C => D => E] = map2(a, b){(a, b) => f.curried(a)(b)}
      val solution2 = map3(c, d, cde){ (c, d, cde) => cde(c)(d)}
      solution2
      //solution1
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
    def sum(ints: IndexedSeq[Int]): Par[Int] = fold(ints)(0)(_ + _)

    def max(ints: IndexedSeq[Int]): Par[Int] = fold(ints)(Integer.MIN_VALUE)(Math.max)

    def nbOfWords(paragraphs: List[String]): Par[Int] = {
      def wordCount(p: String): Int = "\\w+".r.findAllIn(p).size
      traverse(paragraphs.toIndexedSeq, 0)(wordCount)( _ + _)
    }

    def traverse[A, B](seq: IndexedSeq[A], zero: B)(f: A => B)(append: (B, B) => B): Par[B] = {
      val seqpb: IndexedSeq[Par[B]] = seq.map(asyncF(f))
      val ppb: Par[Par[B]] = fold(seqpb)(unit(zero)){ (l, r) =>
        map2(l, r)(append)
      }
      flatten(ppb)
    }



    def flatten[A](p: Par[Par[A]]): Par[A] = { es: ExecutorService =>
      run(es)(p).get._1(es)
    }


    def fold[A](seq: IndexedSeq[A])(zero: A)(op: (A, A) => A): Par[A] = {
      if (seq.size <= 1)
        Par.unit(seq.headOption getOrElse zero)
      else {
        val (l, r) = seq.splitAt(seq.length / 2)
        val pl = Par.fork(fold(l)(zero)(op))
        val pr = Par.fork(fold(r)(zero)(op))

        map2(pl, pr) {op}
      }
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

    // Excercise 7
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
