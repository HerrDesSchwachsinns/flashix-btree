package test

import datatypes.zbranch
import helpers.scala.ArrayWrapperDeep
import datatypes.key.inodekey

object OtherTests {
  def main(args: Array[String]) {
    val a = new ArrayWrapperDeep[zbranch](8)
    for (i <- 0 to 7)
      a(i) = zbranch(inodekey(i), i, null)
    val b = a.deepCopy
    println("BLA")
  }
}