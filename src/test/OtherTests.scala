package test

import datatypes.zbranch
import helpers.scala.ArrayWrapperDeep
import datatypes.key.inodekey
import layers.BtreeIter
import layers.drawBtree
import java.io.PrintWriter
import java.io.FileWriter
import java.io.BufferedWriter

object OtherTests {
  def main(args: Array[String]) {
    val btree = new BtreeIter
    for(i <- 1 to 50) {
      Helper.insert(btree,i)
    }
    btree.draw("test.tex")
  }
}