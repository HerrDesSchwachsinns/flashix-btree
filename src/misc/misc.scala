import datatypes.key
import datatypes.key.inodekey
import datatypes.znode
import helpers.scala.ArrayWrapperDeep
import helpers.scala.Boolean._
import helpers.scala.Boolean
import helpers.scala.Int._
import helpers.scala.Random._
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
  def default_key(): key = inodekey(uninit_address)
  //no key, uninit_address, no child
  def default_zbranches = {
    val array = new ArrayWrapperDeep[zbranch](BRANCH_SIZE)
    array.fill(zbranch.mkZbranch(default_key(),uninit_address,null)) //TODO
    array
  }
  /**
   * create default objects after allocation,
   * note that the default implementation restrictions for unspecified functions apply (see below)
   */
  //no parent, no next, array of branches, leaf, not dirty, initial no elem used
  def default_znode: znode = new znode(null, null, default_zbranches, true, false, 0)
  //default znode, empty map
//  def empty_Btree: Btree = new Btree(default_znode,new MapWrapperDeep[address, index_node])
  /**
   * Unspecified constants
   */
  def ADR_DUMMY: address = 0xDEADBEEF
  def KEY_DUMMY: key = inodekey(0xDEADBEFF)
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
  import datatypes.key.datakey
  import datatypes.key.dentrykey
  import datatypes.key.inodekey
//  def <(param0: key, param1: key): Boolean = { param0.ino < param1.ino }
//  implicit object keyOrdering extends Ordering[key] { //TODO implement for all key types
//    def compare(key0: key, key1: key): Int = key0.ino - key1.ino
//  }
//  val keyOrderingTest = implicitly[Ordering[key]]
  //inodekey > dentrykey > datakey
  implicit class orderedKey(k: key) extends Ordered[key] {

    override def compare(that: key) = (this.k, that) match {
      case (inodekey(ino0), inodekey(ino1)) => implicitly[Ordering[Int]].compare(ino0, ino1)
      case (inodekey(ino), _) => 1
      case (_, inodekey(ino)) => -1
      case (dentrykey(ino0, name0), dentrykey(ino1, name1)) =>
        implicitly[Ordering[(Int, String)]]
          .compare((ino0, name0), (ino1, name1))
      case (dentrykey(ino, name), _) => 1
      case (_, dentrykey(ino, name)) => -1
      case (datakey(ino0, part0), datakey(ino1, part1)) =>
        implicitly[Ordering[(Int, Int)]]
          .compare((ino0, part0), (ino1, part1))
    }
  }
}
