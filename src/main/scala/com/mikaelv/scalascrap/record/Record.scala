package com.mikaelv.scalascrap.record

import scalaz.Monoid
import shapeless._

/** Conjunction of two types, can be used in record type parameter.
  * This could be useful to extract the Encoder type classes with implicits.
  * The big problem is that we add fields in the right order, as opposed to the "with" approach */
trait /\[A,B]

/**
 * Flexible record implementation.
  */
case class Record[F](private val fields: Map[RecordKey, Any]) {
  private[record] def getByKey(key: RecordKey): Any = fields(key)
  
  /** Adds a field. Pass an Option[T] if the field is optional */
  def add[T](value: T)(implicit k: RecordKeyProvider[T]) : Record[F with T] = new Record[F with T](fields + (k.key -> value))

  /** Add all fields from another record */
  def addAll[F2](rec: Record[F2]): Record[F with F2] = new Record[F with F2](fields ++ rec.fields)

  def shrink[F2]: Record[F2] = this.asInstanceOf[Record[F2]]

  
  def get[T](implicit k: RecordKeyProvider[T], ev: F <:< T): T = fields(k.key).asInstanceOf[T]


}

object Record {
  trait Encodable[E] {
    def encode: E
  }
  def apply[T](t: T)(implicit k: RecordKeyProvider[T]): Record[T] = new Record(Map(k.key -> t))

  def convertToEncodable[F, E](record: Record[F])(implicit encodeFormat: Monoid[E], encoder: RecordEncoder[F, E]) = new Encodable[E] {
    override def encode: E = encoder.encode(record)
  }
}

case class RecordKey(value: String)

/** Type class that gives a key of a given type, for usage in the Record Map */
case class RecordKeyProvider[T](key: RecordKey)

object RecordKeyProvider {
  /** Provides the implicit value for RecordKey[Option[T]] when the implicit value for RecordKey[T] is in scope */
  implicit def convertToOption[T](implicit rk: RecordKeyProvider[T]): RecordKeyProvider[Option[T]] = new RecordKeyProvider[Option[T]](RecordKey(rk.key.value))
  implicit def convertToSome[T](implicit rk: RecordKeyProvider[T]): RecordKeyProvider[Some[T]] = new RecordKeyProvider[Some[T]](RecordKey(rk.key.value))
}

/** HList to/from Record for a single type */
class RecordGeneric0[A: RecordKeyProvider] extends Generic[Record[A]] {
  override type Repr = A :: HNil

  override def from(r: Repr): Record[A] = Record(r.head)

  override def to(t: Record[A]): Repr = t.get[A] :: HNil

  def add[B : RecordKeyProvider]: Generic[Record[B with A]] = new RecordGeneric[B, A](this)
}

/** HList to/from Record using a chained generic for the tail */
class RecordGeneric[H : RecordKeyProvider, T](val tailGeneric: Generic[Record[T]] {type Repr <: HList}) extends Generic[Record[H with T]] {
  override type Repr = H :: tailGeneric.Repr

  override def from(r: Repr): Record[H with T] = Record(r.head).addAll[T](tailGeneric.from(r.tail))

  override def to(t: Record[H with T]): Repr = t.get[H] :: tailGeneric.to(t.shrink[T])

  def add[B : RecordKeyProvider]: Generic[Record[B with H with T]] = new RecordGeneric[B, H with T](this)
}

object RecordGeneric {
  def apply[A : RecordKeyProvider] = new RecordGeneric0[A]
}

























