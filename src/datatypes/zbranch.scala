package datatypes

import helpers.scala.DeepCopyable
import helpers.scala.InvalidSelector
import helpers.scala.InvalidSelectorUpdate
import helpers.scala.Random._
import misc._
import misc.address

final case class zbranch(var key: key, var adr: address, var child: znode) extends DeepCopyable[zbranch] {
  override def deepCopy(): zbranch = zbranch(key, adr, child)
  override def toString(): String = "zbranch("+key+","+child+")"
}

object zbranch {
  /**
   * Functions for constructors
   */
  def mkZbranch(key: key, adr: address, child: znode) : zbranch = {
    zbranch(key, adr, child)
  }

  implicit object Randomizer extends helpers.scala.Randomizer[zbranch] {
    def random() : zbranch = zbranch(helpers.scala.Random[key], helpers.scala.Random[address], helpers.scala.Random[znode])
  }
}
