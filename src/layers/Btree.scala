package layers

import datatypes.branch
import datatypes.index_node
import datatypes.key
import datatypes.zbranch
import datatypes.znode
import helpers.scala.ArrayWrapper
import helpers.scala.ArrayWrapperDeep
import helpers.scala.Boolean._
import helpers.scala.Boolean
import helpers.scala.ChooseIn
import helpers.scala.ChooseIndex
import helpers.scala.ChooseNotin
import helpers.scala.ChooseRandom
import helpers.scala.Int._
import helpers.scala.MapWrapper
import helpers.scala.MapWrapperDeep
import helpers.scala.Or
import helpers.scala.Random._
import helpers.scala.Ref
import misc._
import misc.address

class Btree(private var ROOT : znode, private val FS : MapWrapperDeep[address, index_node]) {
  private def branchesToZbranches(BRAR : ArrayWrapperDeep[branch], ZBRAR : ArrayWrapperDeep[zbranch])  {
    ZBRAR := new ArrayWrapperDeep[zbranch](BRAR.length)
    val I : Int = 0
    while (I < BRAR.length) {
      ZBRAR(I) = zbranch.mkZbranch(BRAR(I).key, BRAR(I).adr, null)
    }
  }
  private def check_branch(R : znode, I : Int)  {
    val ZBR : zbranch = R.zbranches(I).deepCopy
    if (ZBR.child == null) {
      val ZBR0 : Ref[znode] = ZBR.child
      loadIndexnode(ZBR.adr, ZBR0)
      ZBR.child = ZBR0.get.deepCopy
      ZBR.child.parent = R
      R.zbranches(I) = ZBR.deepCopy
    }
  }
  def commit(ROOT : znode, ADR : Ref[address])  {
    if (ROOT.dirty) {
      if (! ROOT.leaf) {
        var I : Int = 0
        while (I < ROOT.usedsize) {
          if (ROOT.zbranches(I).child.dirty) {
            commit(ROOT.zbranches(I).child, ADR.get)
            ROOT.zbranches(I).adr = ADR.get
          }
          I = I + 1
        }
      }
    }
    saveIndexnode(ROOT, ADR.get)
    ROOT.dirty = false
  }
  def delete(KEY : key)  {
    val FOUND = new Ref[Boolean](false)
    val R = new Ref[znode](null)
    val ADR = new Ref[address](misc.uninit_address())
    lookup_loop(KEY, R.get, ADR.get, FOUND.get)
    if (FOUND.get) {
      delete_rec(R.get, KEY)
    }
  }
  private def delete_branch(R : znode, KEY : key)  {
    val POS = new Ref[Int](0)
    searchPosition(R, KEY, POS.get)
    move_branches_left(R, POS.get)
    R.usedsize = R.usedsize - 1
    mark_dirty(R)
  }
  def delete_iter(KEY : key)  {
    val FOUND = new Ref[Boolean](false)
    val R = new Ref[znode](null)
    val ADR = new Ref[address](misc.uninit_address())
    lookup_loop(KEY, R.get, ADR.get, FOUND.get)
    if (FOUND.get) {
      delete_loop(R.get, KEY)
    }
  }
  private def delete_loop(__R : znode, __KEY : key)  {
    var KEY = __KEY
    var R = __R
    var DONE : Boolean = false
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
  private def delete_rec(R : znode, KEY : key)  {
    delete_branch(R, KEY)
    if (R.parent != null && R.usedsize < MIN_SIZE) {
      var DONE : Boolean = false
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
  private def getParameter(R : znode, POS : Ref[Int], ADR : Ref[address], KEY : Ref[key])  {
    var I : Int = 0
    if (R.parent != null)
      while (I < R.parent.usedsize) {
        I = I + 1
        if (R.parent.zbranches(I).child == R) {
          ADR := R.parent.zbranches(I).adr
          KEY := R.parent.zbranches(I).key
          POS := I
        }
      }
    
  }
  private def getPosition(R : znode, POS : Ref[Int])  {
    val KEY = new Ref[key](null)
    val ADR = new Ref[address](misc.uninit_address())
    getParameter(R, POS.get, ADR.get, KEY.get)
  }
  def insert(KEY : key, ADR : address)  {
    val FOUND = new Ref[Boolean](false)
    val R = new Ref[znode](null)
    val ADR0 = new Ref[address](misc.uninit_address())
    lookup_loop(KEY, R.get, ADR0.get, FOUND.get)
    if (FOUND.get != true) {
      insert_rec(R.get, null, KEY, ADR)
    }
  }
  private def insert_branch(R : znode, CHILD : znode, KEY : key, ADR : address)  {
    var I : Int = if (R.leaf) 0 else 1
    while (I < R.usedsize) {
      if (<(KEY, R.zbranches(I).key)) {
        move_branches_right(R, I)
        R.zbranches(I) = zbranch.mkZbranch(KEY, ADR, CHILD)
        R.usedsize = R.usedsize + 1
        if (R.leaf) {
          mark_dirty(R)
        }
        I = R.usedsize
      } else
        I = I + 1
    }
  }
  def insert_iter(KEY : key, ADR : address)  {
    val FOUND = new Ref[Boolean](false)
    val R = new Ref[znode](null)
    val ADR0 = new Ref[address](misc.uninit_address())
    lookup_loop(KEY, R.get, ADR0.get, FOUND.get)
    if (FOUND.get != true) {
      insert_loop(R.get, null, KEY, ADR)
    }
  }
  private def insert_loop(__R : znode, __CHILD : znode, __KEY : key, __ADR : address)  {
    var ADR = __ADR
    var KEY = __KEY
    var CHILD = __CHILD
    var R = __R
    var DONE : Boolean = false
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
  private def insert_rec(R : znode, CHILD : znode, KEY : key, ADR : address)  {
    if (R.usedsize < BRANCH_SIZE) {
      insert_branch(R, CHILD, KEY, ADR)
    } else {
      val R0 = new Ref[znode](null)
      split(R, CHILD, KEY, ADR, R0.get)
      if (R.parent != null) {
        if (R0.get.leaf) {
          insert_rec(R.parent, R0.get, R.zbranches(R.usedsize - 1).key, ADR_DUMMY)
        } else {
          insert_rec(R.parent, R0.get, R0.get.zbranches(0).key, ADR_DUMMY)
        }
      }
    }
  }
  private def loadIndexnode(ADR : address, R : Ref[znode])  {
    val R0 : Ref[znode] = misc.default_znode
    branchesToZbranches(FS(ADR).branches, R0.get.zbranches)
    R0.get.parent = null
    R0.get.next = null
    R0.get.leaf = FS(ADR).leaf
    R0.get.dirty = false
    R0.get.usedsize = FS(ADR).usedsize
    R := R0.get
  }
  def lookup(KEY : key, R : Ref[znode], ADR : Ref[address], FOUND : Ref[Boolean])  {
    FOUND := false
    if (R.get.leaf) {
      lookup_leaf(KEY, R.get, ADR.get, FOUND.get)
    } else {
      lookup_rec(KEY, R.get, ADR.get, FOUND.get)
    }
  }
  private def lookup_leaf(KEY : key, R : znode, ADR : Ref[address], FOUND : Ref[Boolean])  {
    var I : Int = 0
    while (I < R.usedsize && FOUND.get != true) {
      if (R.zbranches(I).key == KEY) {
        ADR := R.zbranches(I).adr
        FOUND := true
      } else
        I = I + 1
    }
  }
  def lookup_loop(KEY : key, R : Ref[znode], ADR : Ref[address], FOUND : Ref[Boolean])  {
    FOUND := false
    var I : Int = 0
    while (! R.get.leaf) {
      if (I == R.get.usedsize || ! <(R.get.zbranches(I + 1).key, KEY)) {
        check_branch(R.get, I)
        R := R.get.zbranches(I).child
        I = 0
      } else
        I = I + 1
    }
    lookup_leaf(KEY, R.get, ADR.get, FOUND.get)
  }
  private def lookup_rec(KEY : key, R : Ref[znode], ADR : Ref[address], FOUND : Ref[Boolean])  {
    var I : Int = 0
    while (I < R.get.usedsize - 1 && FOUND.get != true) {
      if (! <(R.get.zbranches(I + 1).key, KEY)) {
        check_branch(R.get, I)
        val RTEMP = new Ref[znode](R.get.zbranches(I).child)
        lookup(KEY, RTEMP.get, ADR.get, FOUND.get)
        R.get.zbranches(I).child = RTEMP.get
      }
      I = I + 1
    }
    if (<(R.get.zbranches(I).key, KEY) && I == R.get.usedsize) {
      check_branch(R.get, I)
      val RTEMP = new Ref[znode](R.get.zbranches(I).child)
      lookup(KEY, RTEMP.get, ADR.get, FOUND.get)
      R.get.zbranches(I).child = RTEMP.get
    }
  }
  private def mark_dirty(__R : znode)  {
    var R = __R
    while (R != null && ! R.dirty) {
      R.dirty = true
      R = R.parent
    }
  }
  private def merge(RL : znode, RR : znode)  {
    var I : Int = 0
    val POS = new Ref[Int](0)
    getPosition(RR, POS.get)
    RR.zbranches(0).key = RR.parent.zbranches(POS.get).key
    while (I < RR.usedsize) {
      RL.zbranches(RL.usedsize) = RR.zbranches(I).deepCopy
      I = I + 1
      RL.usedsize = RL.usedsize + 1
    }
    RL.dirty = true
    if (RL.parent == ROOT && ROOT.usedsize == 2)
      ROOT = RL
    
  }
  private def move_branches_left(R : znode, __I : Int)  {
    var I = __I
    while (I < R.usedsize - 1) {
      R.zbranches(I) = R.zbranches(I + 1).deepCopy
      I = I + 1
    }
  }
  private def move_branches_right(R : znode, I : Int)  {
    var J : Int = R.usedsize
    while (J >= I) {
      R.zbranches(J + 1) = R.zbranches(J).deepCopy
      J = J - 1
    }
  }
  private def move_from_left(R : znode, RL : znode)  {
    move_branches_right(R, 0)
    R.zbranches(0) = RL.zbranches(RL.usedsize - 1).deepCopy
    R.usedsize = R.usedsize + 1
    RL.usedsize = RL.usedsize - 1
    RL.dirty = true
    val POS = new Ref[Int](0)
    getPosition(R, POS.get)
    R.zbranches(1).key = R.parent.zbranches(POS.get).key
    R.parent.zbranches(POS.get).key = RL.zbranches(RL.usedsize - 1).key
  }
  private def move_from_right(R : znode, RR : znode)  {
    R.zbranches(R.usedsize) = RR.zbranches(0).deepCopy
    move_branches_left(RR, 0)
    R.usedsize = R.usedsize + 1
    RR.usedsize = RR.usedsize - 1
    RR.dirty = true
    val POS = new Ref[Int](0)
    getPosition(RR, POS.get)
    R.zbranches(R.usedsize - 1).key = RR.parent.zbranches(POS.get).key
    RR.parent.zbranches(POS.get).key = RR.zbranches(0).key
  }
  private def saveIndexnode(R : znode, ADR : Ref[address])  {
    val IND : index_node = null
    ChooseNotin((FS).keys.toSeq, (ADR0 : address) =>
    {
      ADR := ADR0
      zbranchesToBranches(R.zbranches, IND.branches)
      IND.leaf = R.leaf
      IND.usedsize = R.usedsize
      FS(ADR0) = IND.deepCopy
    })
  }
  private def searchPosition(R : znode, KEY : key, POS : Ref[Int])  {
    var J : Int = 0
    while (J < R.usedsize) {
      if (R.zbranches(J).key == KEY)
        POS := J
      
      J = J + 1
    }
  }
  private def split(R : znode, CHILD : znode, KEY : key, ADR : address, R0 : Ref[znode])  {
    val R1 : znode = misc.default_znode
    R0 := R1
    R0.get.leaf = R.leaf
    R0.get.parent = R.parent
    R0.get.next = R.next
    R.next = R0.get
    R0.get.dirty = true
    R.dirty = true
    if (! <(R.zbranches(MIN_SIZE).key, KEY)) {
      split_branch(R, R0.get, MIN_SIZE)
      insert_branch(R, CHILD, KEY, ADR)
    } else {
      split_branch(R, R0.get, MIN_SIZE + 1)
      insert_branch(R0.get, CHILD, KEY, ADR)
    }
    if (R.parent == null) {
      val ROOT : znode = misc.default_znode
      ROOT.leaf = false
      ROOT.parent = null
      ROOT.next = null
      ROOT.dirty = true
      ROOT.usedsize = 2
      R0.get.parent = ROOT
      R.parent = ROOT
      val ZBRAR : ArrayWrapperDeep[zbranch] = new ArrayWrapperDeep[zbranch](BRANCH_SIZE)
      ZBRAR(0) = zbranch.mkZbranch(KEY, ADR_DUMMY, R)
      ZBRAR(1) = zbranch.mkZbranch(R0.get.zbranches(0).key, ADR_DUMMY, R0.get)
      ROOT.zbranches = ZBRAR.deepCopy
    }
  }
  private def split_branch(R : znode, R0 : znode, I : Int)  {
    var K : Int = 0
    val ZBRAR : ArrayWrapperDeep[zbranch] = new ArrayWrapperDeep[zbranch](BRANCH_SIZE)
    var J : Int = I
    while (J < BRANCH_SIZE) {
      ZBRAR(K) = R.zbranches(J).deepCopy
      if (! R.leaf)
        ZBRAR(K).child.parent = R0
      
      K = K + 1
      J = J + 1
    }
    R0.zbranches = ZBRAR.deepCopy
    R.usedsize = I
    R0.usedsize = K
  }
  private def zbranchesToBranches(ZBRAR : ArrayWrapperDeep[zbranch], BRAR : ArrayWrapperDeep[branch])  {
    BRAR := new ArrayWrapperDeep[branch](ZBRAR.length)
    val I : Int = 0
    while (I < ZBRAR.length) {
      BRAR(I) = branch.mkBranch(ZBRAR(I).key, ZBRAR(I).adr)
    }
  }
}

