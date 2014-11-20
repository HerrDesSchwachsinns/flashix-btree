package layers

import datatypes.key
import helpers.scala.Ref
import misc.address

class BtreeMap(var map: scala.collection.mutable.Map[key, address]) extends IBtree {
  def this() = this(scala.collection.mutable.Map())
  override def insert(KEY: key, ADR: address) {
	  map += (KEY -> ADR)
  }
  override def delete(KEY: key) {
	  map -= KEY
  }
  override def lookup(KEY: key, ADR: Ref[address], FOUND: Ref[Boolean]) {
	FOUND := map contains(KEY)
	if(FOUND.get == true) ADR := map(KEY)
  }
}