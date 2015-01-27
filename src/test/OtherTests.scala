package test

import datatypes.zbranch
import helpers.scala.ArrayWrapperDeep
import datatypes.key.inodekey
import layers.BtreeIter
import layers.BtreeRec
import layers.drawBtree
import java.io.PrintWriter
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.InputStreamReader
import java.io.BufferedReader
import misc.Exec
import scala.util.Random

object OtherTests {
  def main(args: Array[String]) {
    val tree = new BtreeRec
//    val randomVals = List.fill(40)(Random.nextInt(100)).distinct
    val randomVals = List(51, 78, 8, 55, 96, 23, 92, 33, 36, 44, 45, 98, 87, 57, 52, 86, 80, 35, 28, 43, 42, 10, 47, 88, 22, 77, 40, 24, 61, 2, 37, 31, 67, 68, 85, 26)
    println(randomVals)
    for(i <- randomVals) {
      Helper.insert(tree,i)
      tree.draw("test.tex")
    }
  }
}