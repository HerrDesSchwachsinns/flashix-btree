package test

import layers.BtreeBase
import layers.BtreeIter
import layers.BtreeRec
import datatypes.key.inodekey
import layers.BtreeMap
import helpers.scala.Ref
import misc.address
import misc.ADR_DUMMY

object TestInsert {
  def main(args: Array[String]) {
    val treeIter = new BtreeIter
    println("iterative version started")
    test(treeIter)
    println("iterative version finished")
    println("recursive version started")
    val treeRec = new BtreeRec
    test(treeRec)
    println("recursive version finished")
  }
  def test(btree: BtreeBase) {
	  btree.insert(inodekey(1),1)
  }
}