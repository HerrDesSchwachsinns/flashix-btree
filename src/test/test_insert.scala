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
    val map = new BtreeMap
    map.insert(inodekey(1), 1)
    val found = new Ref[Boolean](false)
    val adr = new Ref[address](ADR_DUMMY)
    map.lookup(inodekey(1), adr, found)
    println(found.get)
    map.delete(inodekey(1))
    map.lookup(inodekey(1), adr, found)
    println(found.get)
//    val treeIter = new BtreeIter
//    test(treeIter)
//    val treeRec = new BtreeRec
//    test(treeRec)
  }
  def test(btree: BtreeBase) {
	  btree.insert(inodekey(1),1)
  }
}