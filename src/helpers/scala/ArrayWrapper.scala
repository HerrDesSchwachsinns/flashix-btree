package helpers.scala

import scala.reflect.ClassTag

/**
 * Wrapper around mutable arrays
 *  @note We use the wrapper to be able to exchange the array if changing the
 *        actual object is not possible (e.g. because the size could/would change)
 */
sealed abstract class ArrayWrapperBase[T] protected (var array: Array[T]) {
  final def length: Int = array.length
  final def apply(idx: Int): T = array(idx)
  final def update(idx: Int, value: T): Unit = array(idx) = value
  final override def equals(a: Any): Boolean = a.isInstanceOf[ArrayWrapper[_]] && a.asInstanceOf[ArrayWrapper[_]].array.deep == array.deep

  def :=(wrapper: ArrayWrapperBase[T]) {
    array = wrapper.array
  }
  override def toString(): String = {
    array.mkString("[",", ","]")
  }

}

/** Wrapper around mutable arrays, if T does not contain anything mutable */
final class ArrayWrapper[T : ClassTag](_array: Array[T]) extends ArrayWrapperBase[T](_array) with DeepCopyable[ArrayWrapper[T]] {

  def this(length: Int) = this(new Array[T](length))

  def fill(elem: T): Unit = {
    var i = 0
    while (i < array.size) {
      array(i) = elem
      i = i + 1
    }
  }

  def fill(elem: T, idx: Int, size: Int): Unit = {
    var i = 0
    while (i < size) {
      array(idx + i) = elem
      i = i + 1
    }
  }

  def copy(src: ArrayWrapper[T], srcPos: Int, destPos: Int, length: Int): Unit = {
    System.arraycopy(src.array, srcPos, array, destPos, length)
  }

  override def deepCopy(): ArrayWrapper[T] = {
    new ArrayWrapper(array.clone())
  }
}

/** wrapper around mutable arrays, if T contains something mutable */
final class ArrayWrapperDeep[T <: DeepCopyable[T] : ClassTag](_array: Array[T]) extends ArrayWrapperBase[T](_array) with DeepCopyable[ArrayWrapperDeep[T]] {

  def this(length: Int) = this(new Array[T](length))

  /** @note we have to perform a deep copy on every element individually, otherwise
   *        the array elements itself would share */
  def fill(elem: T): Unit = {
    var i = 0
    while (i < array.size) {
      array(i) = elem.deepCopy
      i = i + 1
    }
  }

  override def deepCopy(): ArrayWrapperDeep[T] = {
    val newArray = new Array[T](array.length)
    for (i <- 0 until array.length)
      if(array(i) != null)
    	  newArray(i) = array(i).deepCopy
    new ArrayWrapperDeep(newArray)
  }
}

/**
 * Immutable functions on arrays, i.e., copy
 */
object ArrayWrapper {
  def updated[T : ClassTag](wrapper: ArrayWrapper[T], idx: Int, value: T): ArrayWrapper[T] = {
    val newArray = wrapper.array.clone()
    newArray(idx) = value
    new ArrayWrapper(newArray)
  }

  def subarray[T : ClassTag](wrapper: ArrayWrapper[T], begin: Int, size: Int): ArrayWrapper[T] = {
    val newArray = wrapper.array.slice(begin, begin + size)
    new ArrayWrapper(newArray)
  }

  def copy[T : ClassTag](src: ArrayWrapper[T], srcPos: Int, dst : ArrayWrapper[T], destPos : Int, length: Int): ArrayWrapper[T] = {
    val newArray = dst.array.clone()
    System.arraycopy(src.array, srcPos, newArray, destPos, length)
    new ArrayWrapper(newArray)
  }

  def fill[T : ClassTag](wrapper: ArrayWrapper[T], elem : T) : ArrayWrapper[T] = {
    val result = new ArrayWrapper(wrapper.length)
    result.fill(elem)
    result
  }

  def fill[T <: DeepCopyable[T] : ClassTag](wrapper: ArrayWrapperDeep[T], elem : T) : ArrayWrapperDeep[T] = {
    val result = new ArrayWrapperDeep(wrapper.length)
    result.fill(elem)
    result
  }
}
