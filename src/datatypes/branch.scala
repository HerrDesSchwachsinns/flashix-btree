package datatypes

import helpers.scala.DeepCopyable
import helpers.scala.InvalidSelector
import helpers.scala.InvalidSelectorUpdate
import helpers.scala.Random._
import misc.address

final case class branch(var key: key, var adr: address) extends DeepCopyable[branch] {
  override def deepCopy(): branch = branch(key, adr)
}

object branch {
  /**
   * Functions for constructors
   */
  def mkBranch(key: key, adr: address) : branch = {
    branch(key, adr)
  }

  implicit object Randomizer extends helpers.scala.Randomizer[branch] {
    def random() : branch = branch(helpers.scala.Random[key], helpers.scala.Random[address])
  }
}
