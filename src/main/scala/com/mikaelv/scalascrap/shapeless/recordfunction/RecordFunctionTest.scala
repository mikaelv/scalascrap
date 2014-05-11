package com.mikaelv.scalascrap.shapeless.recordfunction

import shapeless._, ops.record.Selector, record._, syntax.singleton._

/**
 * Created by mikael on 11/05/2014.
 */
object RecordFunctionTest extends App {
  println(fun1(("foo1"->> "hello") :: ("foo2" ->> 1)::("foo3" ->> 1.2)::HNil))

}
