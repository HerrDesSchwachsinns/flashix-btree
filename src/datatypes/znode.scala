package datatypes

import helpers.scala.ArrayWrapper
import helpers.scala.ArrayWrapperDeep
import helpers.scala.Boolean._
import helpers.scala.Boolean
import helpers.scala.DeepCopyable
import helpers.scala.Int._
import helpers.scala.InvalidSelector
import helpers.scala.InvalidSelectorUpdate
import helpers.scala.Random._
import misc._

final case class znode(var parent: znode, var next: znode, var zbranches: ArrayWrapperDeep[zbranch], var leaf: Boolean, var dirty: Boolean, var usedsize: Int) extends DeepCopyable[znode] {
  override def deepCopy(): znode = znode(parent, next, zbranches.deepCopy, leaf, dirty, usedsize)
  override def toString() = "znode" + zbranches.array.take(usedsize).map(b => b.key).mkString("(", ",", ")")
}

object znode {
  /**
   * Functions for constructors
   */
  def mkZnode(parent: znode, next: znode, zbranches: ArrayWrapperDeep[zbranch], leaf: Boolean, dirty: Boolean, usedsize: Int) : znode = {
    znode(parent, next, zbranches, leaf, dirty, usedsize)
  }

  implicit object Randomizer extends helpers.scala.Randomizer[znode] {
    def random() : znode = znode(helpers.scala.Random[znode], helpers.scala.Random[znode], helpers.scala.Random[ArrayWrapperDeep[zbranch]], helpers.scala.Random[Boolean], helpers.scala.Random[Boolean], helpers.scala.Random[Int])
  }
}
