package com.mikaelv.scalascrap.record

import com.mikaelv.scalascrap.record.SampleDomain._

/** Implementation with beautiful type classes */
object Sendable {
  def send[R, RR](r: Record[R])(implicit sendable: Sendable[R]) {
    val rr = sendable.enrich(r)
    sendable.write(rr)
  }

  abstract class SendableF[R <: A] extends Sendable[R] {


    def superenrich(r: Record[R]) = {
      r.add[E](new E)
    }

    /* cannot implement:
    override def enrich(r: Entity[R]) = {
      r.add(new E)
    }
    This is because the return type will not be an RR.
    This is logical, as RR means all fields have been enriched
    */
    override def write(r: Record[RR]) { println (r) }
  }

  implicit object SendableF1 extends SendableF[F1] {
    type RR = Enrich[F1]

    override def enrich(r: Record[F1]): Record[RR] = {
      val rr = superenrich(r)
      val b: B = r.get[B]
      rr.add[C](C("C for F1: "+ b))
    }
  }

  implicit object SendableF2 extends SendableF[F2] {
    type RR = Enrich[F2]

    override def enrich(r: Record[F2]): Record[RR] = {
      val rr = superenrich(r)
      val d: D = r.get[D]
      rr.add[C](C("C for F2: "+ d))
    }
  }


}

trait Sendable[R] {
  type RR <: R
  def enrich(r: Record[R]): Record[RR]
  def write(r: Record[RR])

}