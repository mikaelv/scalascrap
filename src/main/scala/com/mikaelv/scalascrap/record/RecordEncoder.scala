package com.mikaelv.scalascrap.record


trait EncoderMonoid[E] {
  def op(e1: E, e2: E): E
  def zero: E
}

/** Type class that encodes a type T into a E */
trait Encoder[T, E]  {
  def encode(t: T): E

}



/** Encodes a Record into an arbitrary format defined by:
  * - an Encoder instance for each type composing the record
  * - a EncoderMonoid instance for combining the Encoder results
  */
class RecordEncoder[Fields, E : EncoderMonoid](private val fieldEncoders: Map[RecordKey, Encoder[_, E]]) {


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
  def apply[T, E](implicit e: Encoder[T, E], k: RecordKeyProvider[T], monoid: EncoderMonoid[E]) = new RecordEncoder[T, E](Map(k.key -> e))


  /*implicit def makeRecordEncoder0[E](implicit monoid: EncoderMonoid[E]): RecordEncoder[Unit, E] =
  new RecordEncoder[Unit, E](Map.empty)*/

  // TODO: =!= constaint disambiguates the implicit, but possibly too much, as the conversion does not happen
  implicit def makeRecordEncoder1[T, F, E](re0: RecordEncoder[F, E])(implicit ev: T =!= F, encoder: Encoder[T, E], keyp: RecordKeyProvider[T], monoid: EncoderMonoid[E]):
  RecordEncoder[F with T, E] =
    re0.add[T]
}
