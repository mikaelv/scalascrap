package com.mikaelv.scalascrap.fpinscala

/**
 * Created by mikael on 09/06/2014.
 */
object Chapter4Errors extends App {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) map { m =>
      xs map (x => Math.pow(x - m, 2))
    } flatMap mean
  }


  println(variance(List(1,2,3,4)))
  println(mean(List(1,2,3,4)))

}
