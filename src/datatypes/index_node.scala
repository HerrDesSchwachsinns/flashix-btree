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

final case class index_node(var branches: ArrayWrapperDeep[branch], var leaf: Boolean, var usedsize: Int) extends DeepCopyable[index_node] {
  override def deepCopy(): index_node = index_node(branches.deepCopy, leaf, usedsize)
}

object index_node {
  /**
   * Functions for constructors
   */
  def indexnode(branches: ArrayWrapperDeep[branch], leaf: Boolean, usedsize: Int) : index_node = {
    index_node(branches, leaf, usedsize)
  }

  implicit object Randomizer extends helpers.scala.Randomizer[index_node] {
    def random() : index_node = index_node(helpers.scala.Random[ArrayWrapperDeep[branch]], helpers.scala.Random[Boolean], helpers.scala.Random[Int])
  }
}
