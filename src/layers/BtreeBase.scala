package layers

import datatypes.branch
import datatypes.index_node
import datatypes.key
import datatypes.zbranch
import datatypes.znode
import helpers.scala.ArrayWrapperDeep
import helpers.scala.ChooseNotin
import helpers.scala.MapWrapperDeep
import helpers.scala.Ref
import misc.orderedKey
import misc.ADR_DUMMY
import misc.BRANCH_SIZE
import misc.MIN_SIZE
import misc.KEY_DUMMY
import misc.address
import misc.default_znode
import datatypes.key.inodekey
import misc.default_zbranches

abstract class BtreeBase(protected var ROOT: znode, protected val FS: MapWrapperDeep[address, index_node]) extends IBtree {
  /**
   * default initialization this is an empty Btree
   */
  def this() = this(default_znode, new MapWrapperDeep[address, index_node])
  override def toString() = ROOT.toString
  def draw(file:String) = {
    drawBtree.file = file
    drawBtree(ROOT)
  }
  /*
   * **************
   * commit
   * **************
   */
  def commit(ROOT: znode, ADR: Ref[address]) {
    if (ROOT.dirty) {
      if (!ROOT.leaf) {
        var I: Int = 0
        while (I < ROOT.usedsize) {
          if (ROOT.zbranches(I).child.dirty) {
            commit(ROOT.zbranches(I).child, ADR)
            ROOT.zbranches(I).adr = ADR.get
          }
          I = I + 1
        }
      }
    }
    saveIndexnode(ROOT, ADR)
    ROOT.dirty = false
  }
  /**
   * saves node referenced by R into FS at ADR
   */
  private def saveIndexnode(R: znode, ADR: Ref[address]) {
    val IND: index_node = null
    ChooseNotin((FS).keys.toSeq, (ADR0: address) =>
      {
        ADR := ADR0
        zbranchesToBranches(R.zbranches, IND.branches)
        IND.leaf = R.leaf
        IND.usedsize = R.usedsize
        FS(ADR0) = IND.deepCopy
      })
  }
  /**
   * converts a zbranch array to a branch array
   */
  private def zbranchesToBranches(ZBRAR: ArrayWrapperDeep[zbranch], BRAR: ArrayWrapperDeep[branch]) {
    BRAR := new ArrayWrapperDeep[branch](ZBRAR.length)
    val I: Int = 0
    while (I < ZBRAR.length) {
      BRAR(I) = branch.mkBranch(ZBRAR(I).key, ZBRAR(I).adr)
    }
  }

  /*
   * **************
   * insert
   * **************
   */
  protected def insert_branch(R: znode, CHILD: znode, KEY: key, ADR: address) { //used in insert & split
    var I: Int = if (R.leaf) 0 else 1 //dummy node
    while (I < R.usedsize) {
      if (KEY < R.zbranches(I).key) {
        move_branches_right(R, I)
        R.zbranches(I) = zbranch.mkZbranch(KEY, ADR, CHILD)
        R.usedsize = R.usedsize + 1
        if (R.leaf) {
          mark_dirty(R)
        } else {
          assert(R.dirty)
        }
        return //TODO no return
        //        I = R.usedsize
      } else
        I = I + 1
    }
    if (I == R.usedsize) { //bug102 special case if insertion at the end
      R.zbranches(I) = zbranch.mkZbranch(KEY, ADR, CHILD)
      R.usedsize = R.usedsize + 1
    }
  }
  protected def split(R: znode, CHILD: znode, KEY: key, ADR: address, R0: Ref[znode]) { //used in insert
    //copy parameters from R to R0 (everything but branches)
    //and mark dirty
    val R1: znode = misc.default_znode
    R0 := R1
    R0.get.leaf = R.leaf
    R0.get.parent = R.parent
    R0.get.next = R.next
    R.next = R0.get
    R0.get.dirty = true
    R.dirty = true
    //split branch into two halves R being left, R0 being right
    //and insert node into corresponding half
    if (R.zbranches(MIN_SIZE).key >= KEY) {
      //<1>
      //[2,3,4,5,6,7,8,9] ->
      //[<1>,2,3,4] & [5,6,7,8,9]
      split_branch(R, R0.get, MIN_SIZE)
      insert_branch(R, CHILD, KEY, ADR)
    } else {
      //<9>
      //R: [1,2,3,4,5,6,7,8] ->
      //R:[1,2,3,4,5] & R0:[6,7,8,<9>]
      split_branch(R, R0.get, MIN_SIZE + 1)
      insert_branch(R0.get, CHILD, KEY, ADR)
    }
  }
  private def split_branch(R: znode, R0: znode, I: Int) {
    var K: Int = 0
    val ZBRAR: ArrayWrapperDeep[zbranch] = default_zbranches //bug 104
    var J: Int = I
    while (J < BRANCH_SIZE) {
      ZBRAR(K) = R.zbranches(J).deepCopy
      if (!R.leaf)
        ZBRAR(K).child.parent = R0

      K = K + 1
      J = J + 1
    }
    R0.zbranches = ZBRAR.deepCopy
    R.usedsize = I
    R0.usedsize = K
  }

  /*
   * **************
   * delete
   * **************
   */
  protected def delete_branch(R: znode, KEY: key) { //used in delete
    val POS = new Ref[Int](0)
    searchPosition(R, KEY, POS)
    move_branches_left(R, POS.get)
    R.usedsize = R.usedsize - 1
    mark_dirty(R)
  }

  private def searchPosition(R: znode, KEY: key, POS: Ref[Int]) {
    var J: Int = 0
    while (J < R.usedsize) {
      if (R.zbranches(J).key == KEY)
        POS := J

      J = J + 1
    }
  }
  protected def merge(RL: znode, RR: znode) { //used in delete
    var I: Int = 0
    val POS = new Ref[Int](0)
    getPosition(RR, POS)
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
  protected def move_from_left(R: znode, RL: znode) { //used in delete
    move_branches_right(R, 0)
    R.zbranches(0) = RL.zbranches(RL.usedsize - 1).deepCopy
    R.usedsize = R.usedsize + 1
    RL.usedsize = RL.usedsize - 1
    RL.dirty = true
    val POS = new Ref[Int](0)
    getPosition(R, POS)
    R.zbranches(1).key = R.parent.zbranches(POS.get).key
    R.parent.zbranches(POS.get).key = RL.zbranches(RL.usedsize - 1).key
  }
  protected def move_from_right(R: znode, RR: znode) { //used in delete
    R.zbranches(R.usedsize) = RR.zbranches(0).deepCopy
    move_branches_left(RR, 0)
    R.usedsize = R.usedsize + 1
    RR.usedsize = RR.usedsize - 1
    RR.dirty = true
    val POS = new Ref[Int](0)
    getPosition(RR, POS)
    R.zbranches(R.usedsize - 1).key = RR.parent.zbranches(POS.get).key
    RR.parent.zbranches(POS.get).key = RR.zbranches(0).key
  }

  /*
   * **************
   * lookup helper
   * **************
   */
  protected def lookup_leaf(KEY: key, R: znode, ADR: Ref[address], FOUND: Ref[Boolean]) {
    var I: Int = 0
    while (I < R.usedsize && FOUND.get != true) {
      if (R.zbranches(I).key == KEY) {
        ADR := R.zbranches(I).adr
        FOUND := true
      } else
        I = I + 1
    }
  }

  /*
   * **************
   * branch helper
   * **************
   */
  /**
   * checks if i-th branch already loaded if not it is loaded
   */
  protected def check_branch(R: znode, I: Int) { //used in delete & lookup_loop
    val ZBR: zbranch = R.zbranches(I).deepCopy
    if (ZBR.child == null) {
      val ZBR0: Ref[znode] = new Ref(ZBR.child)
      loadIndexnode(ZBR.adr, ZBR0)
      ZBR.child = ZBR0.get.deepCopy
      ZBR.child.parent = R
      R.zbranches(I) = ZBR.deepCopy
    }
  }
  /**
   * lode node from FS at ADR into R
   */
  private def loadIndexnode(ADR: address, R: Ref[znode]) {
    val R0: Ref[znode] = new Ref(misc.default_znode)
    branchesToZbranches(FS(ADR).branches, R0.get.zbranches)
    R0.get.parent = null
    R0.get.next = null
    R0.get.leaf = FS(ADR).leaf
    R0.get.dirty = false
    R0.get.usedsize = FS(ADR).usedsize
    R := R0.get
  }
  /**
   * convert branch array BRAR into a zbranch array saving result in ZBRAR
   */
  private def branchesToZbranches(BRAR: ArrayWrapperDeep[branch], ZBRAR: ArrayWrapperDeep[zbranch]) {
    ZBRAR := new ArrayWrapperDeep[zbranch](BRAR.length)
    val I: Int = 0
    while (I < BRAR.length) {
      ZBRAR(I) = zbranch.mkZbranch(BRAR(I).key, BRAR(I).adr, null)
    }
  }

  /**
   * mark znode dirty, used if node 	changed (insert or delete)
   */
  private def mark_dirty(__R: znode) { //used in {insert,delete}_branch
    var R = __R
    while (R != null && !R.dirty) {
      R.dirty = true
      R = R.parent
    }
  }
  /**
   * return position in zbranch array of parent of R
   */
  protected def getPosition(R: znode, POS: Ref[Int]) { //used in delete & merge & move_from_{left,right}
    val KEY = new Ref[key](null)
    val ADR = new Ref[address](misc.uninit_address())
    getParameter(R, POS, ADR, KEY)
  }
  /**
   * collect information about a znode that is saved in its parent (key, adr, position).
   */
  private def getParameter(R: znode, POS: Ref[Int], ADR: Ref[address], KEY: Ref[key]) {
    var I: Int = 0
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
  protected def move_branches_left(R: znode, __I: Int) { //used in delete_branch & move_from_right
    var I = __I
    while (I < R.usedsize - 1) {
      R.zbranches(I) = R.zbranches(I + 1).deepCopy
      I = I + 1
    }
  }
  protected def move_branches_right(R: znode, I: Int) { //used in insert_branch & move_from_left
    var J: Int = R.usedsize
    assert(J < BRANCH_SIZE)
    while (J >= I) {
      R.zbranches(J + 1) = R.zbranches(J).deepCopy
      J = J - 1
    }
  }
  
  
  /**
   * Utility
   */
  
  /**
   * create new root znode with left and right as childs
   */
  protected def new_root(left:znode,right:znode) {
    val ROOT: znode = misc.default_znode
    ROOT.leaf = false
    ROOT.parent = null
    ROOT.next = null
    ROOT.dirty = true
    ROOT.usedsize = 2
    left.parent = ROOT
    right.parent = ROOT
    val ZBRAR: ArrayWrapperDeep[zbranch] = default_zbranches //bug 104
    ZBRAR(0) = zbranch.mkZbranch(KEY_DUMMY, ADR_DUMMY, left)
    val KEY = if(right.leaf) left.zbranches(left.usedsize - 1).key else right.zbranches(0).key
    ZBRAR(1) = zbranch.mkZbranch(KEY, ADR_DUMMY, right)
    ROOT.zbranches = ZBRAR.deepCopy
    this.ROOT = ROOT //bug 105 TODO
  }
}