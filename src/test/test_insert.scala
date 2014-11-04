package test

import misc.empty_Btree
import misc.ADR_DUMMY
import datatypes.key.inodekey

object TestInsert {
  def main(args: Array[String]) {
    val tree = empty_Btree
    tree.insert(new inodekey(1), ADR_DUMMY)
    /*
     * always fails:
     * insert sets the local ref parameter R to null
     * then calls lookup_loop with this parameter
     * lookup_loop assumes a valid object behind ref which is no the case
     * -> NullPointerException at Btree.scala:288 R.get.leaf (R.get is null)
     */
  }
}