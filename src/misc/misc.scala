import datatypes.key
import datatypes.znode
import helpers.scala.ArrayWrapperDeep
import helpers.scala.Boolean._
import helpers.scala.Boolean
import helpers.scala.Int._
import helpers.scala.Random._
import layers.Btree
import helpers.scala.MapWrapperDeep
import datatypes.index_node

import datatypes.zbranch

package object misc {
  def isSmaller[T](x: T, y: T)(implicit o: Ordering[T]) = o.lt(x, y)

  /**
   * Unspecified sorts
   *
   * implementation restrictions:
   * * uninterpreted sorts must be immutable
   */

  type address = Int
  def uninit_address(): address = ADR_DUMMY
  
  //no key, uninit_address, no child
  def default_zbranches = {
    val array = new ArrayWrapperDeep[zbranch](BRANCH_SIZE)
    array.fill(zbranch.mkZbranch(null,uninit_address,null))
    array
  }
  /**
   * create default objects after allocation,
   * note that the default implementation restrictions for unspecified functions apply (see below)
   */
  //no parent, no next, array of branches, leaf, not dirty, initial no elem used
  def default_znode: znode = new znode(null, null, default_zbranches, true, false, 0)
  //default znode, empty map
  def empty_Btree: Btree = new Btree(default_znode,new MapWrapperDeep[address, index_node])
  /**
   * Unspecified constants
   */
  def ADR_DUMMY: address = 0xDEADBEEF
  def BRANCH_SIZE: Int = 2 * MIN_SIZE //8
  def MIN_SIZE: Int = 4

  /**
   * Unspecified functions
   *
   * implementation restrictions:
   * * algebraic operations must be deterministic (e.g., may not depend on mutable global state)
   * * algebraic operations may not use destructive operations
   * * results of algebraic operations may share memory with their input (but not with [parts of the] mutable global space)
   */
  import helpers.scala.Int.plus1
  import helpers.scala.Set.Ø
  import helpers.scala.Set.++
  def <(param0: key, param1: key): Boolean = { param0.hashCode < param1.hashCode }
  //TODO read about ordering in scala
  //remove this one
  implicit object keyOrdering extends Ordering[key] {
    def compare(key0: key, key1: key): Int = key1.hashCode - key0.hashCode
  }

  val keyOrderingTest = implicitly[Ordering[key]]
}
