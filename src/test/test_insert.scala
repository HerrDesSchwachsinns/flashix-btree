package test

import datatypes.key.inodekey
import misc.orderedKey
import layers.BtreeIter
import layers.BtreeRec
import layers.IBtree
import misc.BRANCH_SIZE
import datatypes.key.datakey
//TODO asserts
object TestInsert {
  def main(args: Array[String]) {
//    println("iterative version started")
//    val treeIter = new BtreeIter
//    test(treeIter)
//    println("iterative version finished")
    println("recursive version started")
    val treeRec = new BtreeRec
    test(treeRec)
    println("recursive version finished")
  }
  def test(btree: IBtree) {
    test_insert_until_split(btree)
//    test_insert_lookup_delete(btree)
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
  def test_insert_until_split(btree: IBtree) {
    for(i <- 1 to BRANCH_SIZE + 1) {
      Helper.insert(btree, i)
    }
    for(i <- 1 to BRANCH_SIZE + 1) {
      println(Helper.lookup(btree, i))
    }
  }
}