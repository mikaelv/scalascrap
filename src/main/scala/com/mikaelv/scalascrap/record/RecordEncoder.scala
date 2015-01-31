package com.mikaelv.scalascrap.record

import scalaz.{Foldable, Monoid}
import scalaz.std.iterable._
import scalaz.syntax.foldable._
import shapeless._


/** Type class that encodes a type T into a E */
trait Encoder[T, E]  {
  def encode(t: T): E

}
trait JsonEncoder[T] extends Encoder[T, String]

object JsonEncoder extends ProductTypeClassCompanion[JsonEncoder] {
  implicit val encoderMonoid = new Monoid[String] {
    override def append(e1: String, e2: => String): String =
      if (e1 == "") e2 else if (e2 == "") e1 else e1 + ", " + e2

    override def zero: String = ""
  }

  implicit object typeClass extends ProductTypeClass[JsonEncoder] {
    override def product[H, T <: HList](ch: JsonEncoder[H], ct: JsonEncoder[T]): JsonEncoder[::[H, T]] = new JsonEncoder[::[H, T]] {
      override def encode(t: ::[H, T]): String = encoderMonoid.append(ch.encode(t.head), ct.encode(t.tail))
    }

    override def emptyProduct: JsonEncoder[HNil] = new JsonEncoder[HNil] {
      override def encode(t: HNil): String = encoderMonoid.zero
    }

    override def project[F, G](instance: => JsonEncoder[G], to: (F) => G, from: (G) => F): JsonEncoder[F] = new JsonEncoder[F] {
      override def encode(t: F): String = instance.encode(to(t))
    }
  }
}



/** Encodes a Record into an arbitrary format defined by:
  * - an Encoder instance for each type composing the record
  * - a EncoderMonoid instance for combining the Encoder results
  */
class RecordEncoder[Fields, E : Monoid](private val fieldEncoders: Map[RecordKey, Encoder[_, E]]) {


  def add[T](implicit encoder: Encoder[T, E], keyp: RecordKeyProvider[T]) =
    new RecordEncoder[Fields with T, E](fieldEncoders + (keyp.key -> encoder))

  def encode(record: Record[Fields]): E = {

    def getEncoder[T](key: RecordKey): Encoder[T, E] = fieldEncoders(key).asInstanceOf[Encoder[T, E]]

    def encodeField[T](key: RecordKey) = getEncoder(key).encode(record.getByKey(key).asInstanceOf[T])

    val encodedFields = fieldEncoders.keys map encodeField

    Foldable[Iterable].fold(encodedFields)
  }
}


object RecordEncoder {
  def apply[T, E](implicit e: Encoder[T, E], k: RecordKeyProvider[T], monoid: Monoid[E]) = new RecordEncoder[T, E](Map(k.key -> e))



  /*implicit def makeRecordEncoder0[E](implicit monoid: EncoderMonoid[E]): RecordEncoder[Unit, E] =
  new RecordEncoder[Unit, E](Map.empty)*/

  // TODO: =!= constaint disambiguates the implicit, but possibly too much, as the conversion does not happen
  /*implicit def makeRecordEncoder1[T, F, E](re0: RecordEncoder[F, E])(implicit ev: T =!= F, encoder: Encoder[T, E], keyp: RecordKeyProvider[T], monoid: EncoderMonoid[E]):
  RecordEncoder[F with T, E] =
    re0.add[T]*/
}
