package com.mikaelv.scalascrap.record



/**
 * Flexible record implementation.
  */
case class Record[+F](private val fields: Map[RecordKey, Any]) {
  private[record] def getByKey(key: RecordKey): Any = fields(key)
  
  /** Adds a field. Pass an Option[T] if the field is optional */
  def add[T](value: T)(implicit k: RecordKeyProvider[T]) : Record[F with T] = new Record[F with T](fields + (k.key -> value))

  
  def get[T](implicit k: RecordKeyProvider[T], ev: F <:< T): T = fields(k.key).asInstanceOf[T]


}

object Record {
  def apply[T](t: T)(implicit k: RecordKeyProvider[T]): Record[T] = new Record(Map(k.key -> t))
}

case class RecordKey(value: String)

/** Type class that gives a key of a given type, for usage in the Record Map */
case class RecordKeyProvider[T](key: RecordKey)

object RecordKeyProvider {
  /** Provides the implicit value for RecordKey[Option[T]] when the implicit value for RecordKey[T] is in scope */
  implicit def convertToOption[T](implicit rk: RecordKeyProvider[T]): RecordKeyProvider[Option[T]] = new RecordKeyProvider[Option[T]](RecordKey(rk.key.value))
  implicit def convertToSome[T](implicit rk: RecordKeyProvider[T]): RecordKeyProvider[Some[T]] = new RecordKeyProvider[Some[T]](RecordKey(rk.key.value))
}

/** Type class that encodes a type T into a E */
trait Encoder[T, E]  {
  def encode(t: T): E

}

trait EncoderMonoid[E] {
  def op(e1: E, e2: E): E
  def zero: E
}


/** Encodes a Record into an arbitrary format defined by:
  * - an Encoder instance for each type composing the record
  * - a EncoderMonoid instance for combining the Encoder results
  */
case class RecordEncoder[Fields, E : EncoderMonoid](private val fieldEncoders: Map[RecordKey, Encoder[_, E]]) {


  def add[T](implicit encoder: Encoder[T, E], keyp: RecordKeyProvider[T]) =
    new RecordEncoder[Fields with T, E](fieldEncoders + (keyp.key -> encoder))
  
  def encode(record: Record[Fields]): E = {
    val monoid = implicitly[EncoderMonoid[E]]

    def getEncoder[T](key: RecordKey): Encoder[T, E] = fieldEncoders(key).asInstanceOf[Encoder[T, E]]

    def encodeField[T](key: RecordKey) = getEncoder(key).encode(record.getByKey(key).asInstanceOf[T])

    val encodedFields = fieldEncoders.keys map encodeField
    encodedFields.fold(monoid.zero)(monoid.op)
  }
}

object RecordEncoder {
  def apply[T, E : EncoderMonoid](implicit encoder: Encoder[T, E], keyp: RecordKeyProvider[T]) =
    new RecordEncoder[T, E](Map(keyp.key -> encoder))
}


















