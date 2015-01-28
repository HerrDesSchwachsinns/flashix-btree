package layers

import datatypes.index_node
import datatypes.key
import datatypes.znode
import helpers.scala.MapWrapperDeep
import helpers.scala.Ref
import misc.orderedKey
import misc.ADR_DUMMY
import misc.BRANCH_SIZE
import misc.MIN_SIZE
import misc.address
import misc.default_znode

class BtreeRec() extends BtreeBase() {
  override def insert(KEY: key, ADR: address) {
    val FOUND = new Ref[Boolean](false)
    val R = new Ref[znode](ROOT)
    val ADR0 = new Ref[address](misc.uninit_address())
    lookup_impl(KEY, R, ADR0, FOUND)
    if (FOUND.get != true) {
      insert_rec(R.get, null, KEY, ADR)
    }
  }
  override def delete(KEY: key) {
    val FOUND = new Ref[Boolean](false)
    val R = new Ref[znode](ROOT)
    val ADR = new Ref[address](misc.uninit_address())
    lookup_impl(KEY, R, ADR, FOUND)
    if (FOUND.get) {
      delete_rec(R.get, KEY)
    }
  }
  override def lookup(KEY: key, ADR: Ref[address], FOUND: Ref[Boolean]) {
    val R = new Ref[znode](ROOT)
    lookup_impl(KEY, R, ADR, FOUND)
  }

  private def insert_rec(R: znode, CHILD: znode, KEY: key, ADR: address) {
    if (R.usedsize < BRANCH_SIZE) {
      insert_branch(R, CHILD, KEY, ADR)
    } else {
      val R0 = new Ref[znode](null)
      split(R, CHILD, KEY, ADR, R0)
      if (R.parent != null) { //is not root
        if (R0.get.leaf) {
          insert_rec(R.parent, R0.get, R.zbranches(R.usedsize - 1).key, ADR_DUMMY)
        } else {
          insert_rec(R.parent, R0.get, R0.get.zbranches(0).key, ADR_DUMMY)
        }
      } else { //R is root
        new_root(R, R0.get);
      }
    }
  }
  private def delete_rec(R: znode, KEY: key) {
    delete_branch(R, KEY)
    if (R.parent != null && R.usedsize < MIN_SIZE) {
      var DONE: Boolean = false
      val POS = new Ref[Int](0)
      getPosition(R, POS)
      if (POS.get > 0) {
        check_branch(R.parent, POS.get - 1)
        if (R.parent.zbranches(POS.get - 1).child.usedsize >= MIN_SIZE + 1) {
          move_from_left(R, R.parent.zbranches(POS.get - 1).child)
          DONE = true
        }
      }
      if (DONE != true && POS.get + 1 < R.parent.usedsize) {
        check_branch(R.parent, POS.get + 1)
        if (R.parent.zbranches(POS.get + 1).child.usedsize >= MIN_SIZE + 1) {
          move_from_right(R, R.parent.zbranches(POS.get + 1).child)
          DONE = true
        }
      }
      if (DONE != true) {
        if (POS.get > 0) {
          merge(R.parent.zbranches(POS.get - 1).child, R)
          if (R.parent != null) {
            delete_rec(R.parent, R.parent.zbranches(POS.get).key)
          }
        } else {
          merge(R, R.parent.zbranches(POS.get + 1).child)
          if (R.parent != null) {
            delete_rec(R.parent, R.parent.zbranches(POS.get + 1).key)
          }
        }
        DONE = true
      }
    }
  }
  private def lookup_impl(KEY: key, R: Ref[znode], ADR: Ref[address], FOUND: Ref[Boolean]) {
    FOUND := false
    if (R.get.leaf) {
      lookup_leaf(KEY, R.get, ADR, FOUND)
    } else {
      lookup_rec(KEY, R, ADR, FOUND)
    }
  }
  private def lookup_rec(KEY: key, R: Ref[znode], ADR: Ref[address], FOUND: Ref[Boolean]) {
    var I: Int = 0
    while (I < R.get.usedsize - 1 && R.get.zbranches(I + 1).key < KEY) {
      I = I + 1
    }
    check_branch(R.get, I)
    val RTEMP = new Ref[znode](R.get.zbranches(I).child)
    lookup_impl(KEY, RTEMP, ADR, FOUND)
    R := RTEMP.get //bug 107
  }
}