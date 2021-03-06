package helpers.scala

import scala.collection.mutable.Map

/** Wrapper around mutable maps */
sealed abstract class MapWrapperBase[K, T] protected (var map: Map[K, T] = Map[K, T]()) {
  def size : Int = map.size
  def contains(key : K) = map.contains(key)
  def keys : Iterable[K] = map.keys
  def keySet : Set[K] = map.keySet.toSet // TODO: Problem wenn der Codegenerator mutable sets nehmen will
  def apply(key : K) : T = map(key)
  def -= (key : K) : Unit = map -= key
  def --= (keys : TraversableOnce[K]) : Unit = map --= keys
  def update(key : K, value : T) : Unit = map += Tuple2(key, value)
  def += (keyvalue : (K, T)) : Unit = map += keyvalue
  def isEmpty: Boolean = map.isEmpty
  override def toString() = map.toString
  /* replace the actual map */
  def := (wrapper: MapWrapperBase[K, T]) {
    map = wrapper.map
  }
}

final class MapWrapper[K, T](__initial_map: Map[K, T] = Map[K, T]()) extends MapWrapperBase[K, T](__initial_map) with DeepCopyable[MapWrapper[K, T]] {
  override def deepCopy(): MapWrapper[K, T] = new MapWrapper[K, T](map.clone())
  override def equals(a : Any) : Boolean = a.isInstanceOf[MapWrapper[_, _]] && a.asInstanceOf[MapWrapper[_, _]].map == map
}

final class MapWrapperDeep[K, T <: DeepCopyable[T]](__initial_map: Map[K, T] = Map[K, T]()) extends MapWrapperBase[K, T](__initial_map) with DeepCopyable[MapWrapperDeep[K, T]] {
  override def deepCopy(): MapWrapperDeep[K, T] = {
    val newWrapper = new MapWrapperDeep[K, T]()
    map.foreach(pair => {
      newWrapper += pair // NOTE: does deepCopy of the value
    })
    newWrapper
  }
  override def equals(a : Any) : Boolean = a.isInstanceOf[MapWrapperDeep[_, _]] && a.asInstanceOf[MapWrapperDeep[_, _]].map == map
}

object MapWrapper {
  def updated[K, T](wrapper : MapWrapper[K, T], key : K, elem : T) = {
    // NOTE: This implementation is only correct if T does not contain any mutable parts
    new MapWrapper(wrapper.map.updated(key, elem))
  }
  def remove[K, T](wrapper : MapWrapper[K, T], key : K) = {
    // NOTE: This implementation is only correct if T does not contain any mutable parts
    val newMap = wrapper.map.clone
    newMap -= key
    new MapWrapper(newMap)
  }
  def remove[K, T](wrapper : MapWrapper[K, T], keys : Set[K]) = {
    // NOTE: This implementation is only correct if T does not contain any mutable parts
    val newMap = wrapper.map.clone
    newMap --= keys
    new MapWrapper(newMap)
  }
}
