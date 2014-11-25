package layers

import datatypes.index_node
import datatypes.key
import datatypes.znode
import helpers.scala.MapWrapperDeep
import helpers.scala.Ref
import misc.default_znode
import misc.address
import misc.ADR_DUMMY
import misc.BRANCH_SIZE
import misc.MIN_SIZE
import misc.<
import misc.uninit_address
class BtreeIter() extends BtreeBase() {
  override def insert(KEY: key, ADR: address) {
    val FOUND = new Ref[Boolean](false)
    val R = new Ref[znode](ROOT)
    val ADR0 = new Ref[address](misc.uninit_address())
    lookup_loop(KEY, R.get, ADR0.get, FOUND.get)
    if (FOUND.get != true) {
      insert_loop(R.get, null, KEY, ADR)
    }
  }
  override def delete(KEY: key) {
    val FOUND = new Ref[Boolean](false)
    val R = new Ref[znode](ROOT)
    val ADR = new Ref[address](misc.uninit_address())
    lookup_loop(KEY, R.get, ADR.get, FOUND.get)
    if (FOUND.get) {
      delete_loop(R.get, KEY)
    }
  }
  override def lookup(KEY: key, ADR: Ref[address], FOUND: Ref[Boolean]) {
    val R = new Ref[znode](ROOT)
    lookup_loop(KEY, R, ADR, FOUND)
  }
  
    private def lookup_loop(KEY: key, R: Ref[znode], ADR: Ref[address], FOUND: Ref[Boolean]) { //used in delete and insert
    FOUND := false
    var I: Int = 0
    while (!R.get.leaf) {
      if (I == R.get.usedsize || ! <(R.get.zbranches(I + 1).key, KEY)) {
        check_branch(R.get, I)
        R := R.get.zbranches(I).child
        I = 0
      } else
        I = I + 1
    }
    lookup_leaf(KEY, R.get, ADR, FOUND)
  }

  private def insert_loop(__R: znode, __CHILD: znode, __KEY: key, __ADR: address) {
    var ADR = __ADR
    var KEY = __KEY
    var CHILD = __CHILD
    var R = __R
    var DONE: Boolean = false
    while (DONE != true) {
      if (R.usedsize < BRANCH_SIZE) {
        insert_branch(R, CHILD, KEY, ADR)
        DONE = true
      } else {
        val R0 = new Ref[znode](null)
        split(R, CHILD, KEY, ADR, R0.get)
        if (R.parent == null)
          DONE = true
        else {
          if (R0.get.leaf)
            KEY = R.zbranches(R.usedsize - 1).key
          else
            KEY = R0.get.zbranches(0).key
          R = R.parent
          CHILD = R0.get
          ADR = ADR_DUMMY
        }
      }
    }
  }
  private def delete_loop(__R: znode, __KEY: key) {
    var KEY = __KEY
    var R = __R
    var DONE: Boolean = false
    while (DONE != true) {
      delete_branch(R, KEY)
      if (R.parent != null && R.usedsize < MIN_SIZE) {
        val POS = new Ref[Int](0)
        getPosition(R, POS.get)
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
              KEY = R.parent.zbranches(POS.get).key
              R = R.parent
            } else
              DONE = true
          } else {
            merge(R, R.parent.zbranches(POS.get + 1).child)
            if (R.parent != null) {
              KEY = R.parent.zbranches(POS.get + 1).key
              R = R.parent
            } else
              DONE = true
          }
        }
      } else
        DONE = true
    }
  }

}