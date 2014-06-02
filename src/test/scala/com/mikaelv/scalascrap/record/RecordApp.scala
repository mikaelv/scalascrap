package com.mikaelv.scalascrap.record

import com.mikaelv.scalascrap.record.SampleDomain._

/**
 * Created by mikael on 01/06/2014.
 */
object RecordApp extends App {


  // Polymorphism: ImplPoly can process an Entity having an A, and if the Entity is a F1, can process the F1 fields

  val e1: Record[F1] = Record(new A).add(B("B value"))
  val e2: Record[F2] = Record(new A).add(D("D value"))
  val ea = Record(new A)
  val edd = Record(new A).add(D("D value")).add(new DD)

  new ImplPoly().send(e1)
  new ImplPoly().send(e2)
  new ImplPoly().send(ea) // should not compile
  new ImplPoly().send(edd) // should not compile
  // should not compile:
  //new ImplPoly().send(Entity.empty.add(new DD))


  println("\nType class implementation test")
  Sendable.send(e1)
  Sendable.send(e2)
  // does not compile :-) Sendable.send(ea)
  // does not compile :-) Sendable.send(edd)

}
