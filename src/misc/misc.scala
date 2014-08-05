import datatypes.key
import datatypes.znode
import helpers.scala.ArrayWrapperDeep
import helpers.scala.Boolean._
import helpers.scala.Boolean
import helpers.scala.Int._
import helpers.scala.Random._

package object misc {
  def isSmaller[T](x: T, y: T)(implicit o: Ordering[T])
    = o.lt(x, y)

  /**
   * Unspecified sorts
   *
   * implementation restrictions:
   * * uninterpreted sorts must be immutable
   */
  
  type address = Int
  def uninit_address() : address = ???

  /**
   * create default objects after allocation,
   * note that the default implementation restrictions for unspecified functions apply (see below)
   */
  def default_znode: znode = ???
  
  /**
   * Unspecified constants
   */
  def ADR_DUMMY : address = 0
  def BRANCH_SIZE : Int = 9
  def MIN_SIZE : Int = 4

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
  def <(param0 : key, param1 : key) : Boolean = { param0.hashCode < param1.hashCode }

  implicit object keyOrdering extends Ordering[key] {
    def compare(key0: key, key1: key): Int
      = key1.hashCode - key0.hashCode
  }
}
