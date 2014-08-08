package com.mikaelv.scalascrap

/**
 * Created by mikael on 16/06/2014.
 */
package object record {
  // http://stackoverflow.com/questions/6909053/enforce-type-difference
  trait =!=[A, B]

  implicit def neq[A, B] : A =!= B = null

  // This pair excludes the A =:= B case
  implicit def neqAmbig1[A] : A =!= A = null
  implicit def neqAmbig2[A] : A =!= A = null



}
