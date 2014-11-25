package test

import datatypes.key.inodekey
import helpers.scala.Ref
import layers.IBtree
import misc.address
import misc.uninit_address

object Helper {
	def insert(btree: IBtree,x:Int) {
	  btree.insert(inodekey(x), x)
	}
	def lookup(btree: IBtree,x:Int):Boolean = {
	  val found = new Ref[Boolean](false)
	  val adr = new Ref[address](uninit_address())
	  btree.lookup(inodekey(x),adr,found)
	  if(found.get && adr.get != x) throw new Exception("address != inodekey")
	  return found.get
	}
	def delete(btree: IBtree,x:Int) {
	  btree.delete(inodekey(x))
	}
}