package com.mikaelv.scalascrap.fpinscala

/**
 * Created by mikael on 23/06/2014.
 */
object Chapter6State {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  type Rand[+A] = RNG => (A, RNG)

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >> 16).toInt
      (n, nextRNG)
    }
  }

  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  val nonNegativeInt: Rand[Int] = { rng =>
    val (i, rng2) = rng.nextInt
    if (i < 0)
      if (i != Int.MinValue)
        (-i, rng2)
      else
        (Int.MaxValue, rng2)
    else
      (i, rng2)

  }


  // Exercise 8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  /*def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else
      nonNegativeLessThan(n)(rng)
  }
*/

  // The state is passed magically ! flatMap looks like a variable assignment
  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      unit(mod)
    else
      nonNegativeLessThan(n)
  }

  // Exercise 9
  def map[A, B](f: Rand[A])(g: A => B): Rand[B] = flatMap(f)(a => unit(g(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) {a =>
    flatMap(rb)(b => unit(f(a, b)))
  }

  val double: Rand[Double] = map(nonNegativeInt) { i => i / Int.MaxValue.toDouble }


  val randIntDouble: Rand[(Int, Double)] = map2(int, double)((_, _))


  // Exercise 11

  // State represents a computation that modifies a context S and returns a value A
  case class State[S, +A](run: S => (A, S)) {

    def map[B](f: A => B): State[S, B] = State { s =>
      val (a, s2) = this.run(s)
      val b = f(a)
      (b, s2)
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
      val (a, s2) = this.run(s)
      f(a).run(s2)
    }
  }
  object State {
    def unit[S, A](a: A): State[S, A] = State { s => (a, s)}

    def modify[S](f: S => S): State[S, Unit] = State { s => (Unit, f(s))}
    def get[S]: State[S, S] = State { s => (s,s)}
    def set[S](f: => S): State[S, Unit] = State { s => (Unit, f)}

    // Exercise 7
    /** Sequence runs a list of computation and returns the list of intermediate results along with the new computation */
    def sequence[S, A](lst: List[State[S, A]]): State[S, List[A]] = {
      val zero: State[S, List[A]] = unit(List.empty)
      lst.foldRight(zero)( (sa, sla) => for { a <- sa; la <- sla } yield a :: la )
    }


    // foldLeft reverses the order, hence we must reverse it first
    def sequence2[S, A](lst: List[State[S, A]]): State[S, List[A]] = {
      val zero: State[S, List[A]] = unit(List.empty)
      lst.reverse.foldLeft(zero)( (sla, sa) => for { a <- sa; la <- sla } yield a :: la )
    }
  }


  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    val unlocked = !locked

    def onInput(input: Input): Machine = input match {
      case Coin if locked && candies > 0 => copy(locked = false, coins = coins +1)
      case Turn if unlocked => copy(locked = true, candies = candies -1)
      case Turn if locked => this
      case Coin if unlocked => this
      case _ if candies == 0 => this
    }

  }

  object Machine {
    def modify(input: Input): State[Machine, Unit] = State.modify(_.onInput(input))
  }

  // This is actually transforming a List of input into a computation
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
      _ <- State.sequence(inputs map Machine.modify )
      machine <- State.get
    } yield (machine.candies, machine.coins)


    //simulatedState.flatMap( _ => State.get.map(machine => (machine.candies, machine.coins)))



}
