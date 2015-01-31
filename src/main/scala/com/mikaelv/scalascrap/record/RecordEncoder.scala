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

/**
 * Automatically creates instances of Encoder[X, E] if there is an instance of Generic[X],
 * and X is a product type whose indivudual components A, B, .. have an instance of Encoder[A, E], Encoder[B, E], ...
 * Use the monoid to add the encoded head and tail
 * @param monoid monoid on the result type of an encoder
 * @tparam E result type of an encoder
 */
class EncoderCompanion[E](monoid: Monoid[E]) extends ProductTypeClassCompanion[({type l[A]=Encoder[A, E]})#l] {
  type EncoderE[X] = Encoder[X, E]
  implicit object typeClass extends ProductTypeClass[EncoderE] {
    override def product[H, T <: HList](ch: EncoderE[H], ct: EncoderE[T]): EncoderE[::[H, T]] = new EncoderE[H :: T] {
      override def encode(t: H :: T): E = monoid.append(ch.encode(t.head), ct.encode(t.tail))
    }

    override def emptyProduct: EncoderE[HNil] = new EncoderE[HNil] {
      override def encode(t: HNil): E = monoid.zero
    }

    override def project[F, G](instance: => EncoderE[G], to: (F) => G, from: (G) => F): EncoderE[F] = new EncoderE[F] {
      override def encode(t: F): E = instance.encode(to(t))
    }
  }
}


object JsonEncoder extends EncoderCompanion[String](
  new Monoid[String] {
    override def append(e1: String, e2: => String): String =
      if (e1 == "") e2 else if (e2 == "") e1 else e1 + ", " + e2

    override def zero: String = ""
  })




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
