package com.mikaelv.scalascrap.scalaz

import scalaz._
import Scalaz._

/**
 * Created by mikael on 09/06/2014.
 */
object Examples extends App {
  val l = List(1,2,3)

  // Simple state modification
  (for {
    _ <- State.put("hello ")
    _ <- State.iModify{s: String => s.size}
  } yield {}).eval("unused")

  // State modification using Monad Transformer
  type Error[+A] = String \/ A
  val s = StateT.stateTMonadState[Double, Error]

  def inverse: StateT[Error, Double, Unit] = StateT {s:Double => if (s == 0) -\/("Division by zero") else \/-(1.0/s,()) }

  def attemptModify[S](f: S => Error[S]) = StateT[Error, S, Unit] { f(_).map((_, ())) }
  def inverse2 = attemptModify { s: Double => if (s == 0) -\/("Division by zero") else \/-(1.0/s) }

  val stateModifications =
    for {
      res <- s.init
      _ <- s.modify(_ * 2)
      _ <- inverse
      _ <- inverse2
      res2 <- s.get
    } yield res2

  println(stateModifications.eval(0))
  println(stateModifications.eval(4))


}
