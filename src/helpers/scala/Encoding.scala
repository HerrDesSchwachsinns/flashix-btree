package helpers.scala

import scala.reflect.ClassTag

/**
 * design decisions:
 * * the array where the data is stored is passed to the encode/decode functions with an index where
 *   en/decoding should start, this allows combining en/decoding functions without array allocations
 * * every decoding function must figure out how many bytes are encoded by itself
 * * encode/decode functions return the new array index
 */
object Encoding {
  def flashsize(value: Boolean): Int = 1
  def flashsize(value: Byte): Int = 1
  def flashsize(value: Short): Int = 2
  def flashsize(value: Int): Int = 3
  def flashsize(value: Long): Int = 4
  def encode(value: Boolean, encoded: Array[Byte], index: Int): Int = {
    encoded(index) = if (value) 1 else 0
    index + 1
  }
  def encode(value: Byte, encoded: Array[Byte], index: Int): Int = {
    encoded(index) = value
    index + 1
  }
  def encode(value: Short, encoded: Array[Byte], index: Int): Int = {
    encoded(index) = value.toByte
    encoded(index + 1) = (value >>> 8).toByte
    index + 2
  }
  def encode(value: Int, encoded: Array[Byte], index: Int): Int = {
    encoded(index) = value.toByte
    encoded(index + 1) = (value >>> 8).toByte
    encoded(index + 2) = (value >>> 16).toByte
    encoded(index + 3) = (value >>> 24).toByte
    index + 4
  }
  def encode(value: Long, encoded: Array[Byte], index: Int): Unit = {
    encoded(index) = value.toByte
    encoded(index + 1) = (value >>> 8).toByte
    encoded(index + 2) = (value >>> 16).toByte
    encoded(index + 3) = (value >>> 24).toByte
    encoded(index + 4) = (value >>> 32).toByte
    encoded(index + 5) = (value >>> 40).toByte
    encoded(index + 6) = (value >>> 48).toByte
    encoded(index + 7) = (value >>> 56).toByte
    index + 8
  }

  def decodeBoolean(encoded: Array[Byte], index: Int): (Boolean, Int) = {
    (encoded(index) == 1, index + 1)
  }
  def decodeByte(encoded: Array[Byte], index: Int): (Byte, Int) = {
    (encoded(index), index + 1)
  }
  def decodeShort(encoded: Array[Byte], index: Int): (Short, Int) = {
    ((encoded(index) | (encoded(index + 1) << 8)).toShort, index + 2)
  }
  def decodeInt(encoded: Array[Byte], index: Int): (Int, Int) = {
    (encoded(index) | (encoded(index + 1) << 8) | (encoded(index + 2) << 16) | (encoded(index + 3) << 24), index + 4)
  }
  def decodeLong(encoded: Array[Byte], index: Int): (Long, Int) = {
    (encoded(index).toLong | (encoded(index + 1).toLong << 8) | (encoded(index + 2).toLong << 16) | (encoded(index + 3).toLong << 24) | (encoded(index + 4).toLong << 32) | (encoded(index + 5).toLong << 40) | (encoded(index + 6).toLong << 48) | (encoded(index + 7).toLong << 56), index + 8)
  }

  /** Calculate how much space the encoded array takes */
  def flashsizeArray[T](array: Array[T], flashsizef: T => Int): Int = {
    // NOTE: 4 byte for the length of the array
    array.array.foldLeft(4)((size, elem) => size + flashsizef(elem))
  }
  /** Encode the array, takes up the space in encoded from "index" to "index + flashsize(array, flashsizef)" */
  def encodeArray[T](array: Array[T], encoded: Array[Byte], index: Int, encodef: (T, Array[Byte], Int) => Int): Int = {
    encode(array.size, encoded, index)
    var curindex = index + 4
    array.foreach(elem => {
      curindex = encodef(elem, encoded, curindex)
    })
    return curindex
  }
  def decodeArray[T: ClassTag](encoded: Array[Byte], index: Int, decodef: (Array[Byte], Int) => (T, Int)): (Array[T], Int) = {
    val (size, newindex) = decodeInt(encoded, index)
    var curindex = newindex
    val array = new Array[T](size)
    var curarrayindex = 0
    while (curarrayindex != size) {
      val (value, newindex) = decodef(encoded, curindex)
      array(curarrayindex) = value
      curindex = newindex
      curarrayindex += 1
    }
    return (array, curindex)
  }

  def flashsizeMap[K, T](map: scala.collection.mutable.Map[K, T], flashsizekeyf: K => Int, flashsizef: T => Int): Int = {
    // NOTE: 4 byte for the number of entries
    map.foldLeft(4)((size, keyelem) => size + flashsizekeyf(keyelem._1) + flashsizef(keyelem._2))
  }
  def encodeMap[K, T](map: scala.collection.mutable.Map[K, T], encoded: Array[Byte], index: Int, encodekeyf: (K, Array[Byte], Int) => Int, encodef: (T, Array[Byte], Int) => Int): Int = {
    encode(map.size, encoded, index)
    var curindex = index + 4
    map.foreach(keyelem => {
      curindex = encodekeyf(keyelem._1, encoded, curindex)
      curindex = encodef(keyelem._2, encoded, curindex)
    })
    return curindex
  }
  def decodeMap[K, T](encoded: Array[Byte], index: Int, decodekeyf: (Array[Byte], Int) => (K, Int), decodef: (Array[Byte], Int) => (T, Int)): (scala.collection.mutable.Map[K, T], Int) = {
    val (size, newindex) = decodeInt(encoded, index)
    var curindex = newindex
    val map = scala.collection.mutable.Map[K, T]()
    var done = 0
    while (done != size) {
      val (key, newindex) = decodekeyf(encoded, curindex)
      val (value, newindex2) = decodef(encoded, newindex)
      map += ((key, value))
      curindex = newindex2
      done += 1
    }
    return (map, curindex)
  }

  def flashsizeArrayWrapper[T](array: ArrayWrapper[T], flashsizef: T => Int): Int = flashsizeArray(array.array, flashsizef)
  def flashsizeArrayWrapperDeep[T <: DeepCopyable[T]](array: ArrayWrapperDeep[T], flashsizef: T => Int): Int = flashsizeArray(array.array, flashsizef)
  def encodeArrayWrapper[T](array: ArrayWrapper[T], encoded: Array[Byte], index: Int, encodef: (T, Array[Byte], Int) => Int): Int = encodeArray(array.array, encoded, index, encodef)
  def encodeArrayWrapperDeep[T <: DeepCopyable[T]](array: ArrayWrapperDeep[T], encoded: Array[Byte], index: Int, encodef: (T, Array[Byte], Int) => Int): Int = encodeArray(array.array, encoded, index, encodef)
  def decodeArrayWrapper[T: ClassTag](encoded: Array[Byte], index: Int, decodef: (Array[Byte], Int) => (T, Int)): (ArrayWrapper[T], Int) = {
    val (array, newindex) = decodeArray(encoded, index, decodef)
    return (new ArrayWrapper[T](array), newindex)
  }
  def decodeArrayWrapperDeep[T <: DeepCopyable[T] : ClassTag](encoded: Array[Byte], index: Int, decodef: (Array[Byte], Int) => (T, Int)): (ArrayWrapperDeep[T], Int) = {
    val (array, newindex) = decodeArray(encoded, index, decodef)
    return (new ArrayWrapperDeep[T](array), newindex)
  }
}
