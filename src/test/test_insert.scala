package test

import datatypes.key.inodekey
import layers.BtreeIter
import layers.BtreeRec
import layers.IBtree

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
  def test(btree: IBtree) {
    test_insert_lookup_delete(btree)
  }
  def test_insert(btree: IBtree) {
    Helper.insert(btree, 1)
  }
  def test_insert_lookup_delete(btree: IBtree) {
    Helper.insert(btree, 1)
    println(Helper.lookup(btree, 1))
    Helper.delete(btree,1)
    println(Helper.lookup(btree, 1))
  }
}