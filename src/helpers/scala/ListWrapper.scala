package helpers.scala

import scala.reflect.ClassTag
import scala.collection.mutable.ListBuffer

/** 
 * Wrapper around mutable lists
 */
sealed abstract class ListWrapperBase[T] protected (var list: ListBuffer[T] = new ListBuffer[T]()) {
  def isEmpty: Boolean = list.isEmpty
  def length: Int = list.length
  def head: T = list.head
  def removeHead: Unit = list.trimStart(1)
  def removeLast: Unit = list.trimEnd(1)
  def contains(elem: T): Boolean = list.contains(elem)

  def += (elem: T): Unit = list += elem
  def +=: (elem: T): Unit = list.+=:(elem)

  /* replace the actual map */
  def := (wrapper: ListWrapperBase[T]) {
    list = wrapper.list
  }
}

final class ListWrapper[T](__initial_list: ListBuffer[T] = new ListBuffer[T]()) extends ListWrapperBase[T](__initial_list) with DeepCopyable[ListWrapper[T]] {
  def this(elem: T) = this(new ListBuffer[T]() += elem)
  def this(elem0: T, elem1: T) = this(new ListBuffer[T]() += elem0 += elem1)

  override def deepCopy(): ListWrapper[T] = new ListWrapper(list.clone())
}

final class ListWrapperDeep[T <: DeepCopyable[T]](__initial_list: ListBuffer[T] = new ListBuffer[T]()) extends ListWrapperBase[T] with DeepCopyable[ListWrapperDeep[T]]{
  def this(elem: T) = this(new ListBuffer[T]() += elem)
  def this(elem0: T, elem1: T) = this(new ListBuffer[T]() += elem0 += elem1)
  override def deepCopy(): ListWrapperDeep[T] = {
    val newWrapper = new ListWrapperDeep[T]()
    list.foreach(elem => {
      newWrapper += elem.deepCopy
    })
    newWrapper
  }
}

/**
 * Immutable functions on arrays, i.e., copy
 */
object ListWrapper {
  def tail[T](wrapper: ListWrapper[T]): ListWrapper[T] = {
    // NOTE: This implementation is only correct if T does not contain any mutable parts
    val newList = wrapper.list.clone
    newList.trimStart(1)
    new ListWrapper[T](newList)
  }

  def prepend[T](elem: T, wrapper: ListWrapper[T]): ListWrapper[T] = {
    // NOTE: This implementation is only correct if T does not contain any mutable parts
    new ListWrapper[T](elem +=: wrapper.list)
  }
}