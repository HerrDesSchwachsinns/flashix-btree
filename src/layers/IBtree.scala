package layers

import datatypes.key
import misc.address
import helpers.scala.Ref

trait IBtree {
	def insert(KEY: key, ADR: address): Unit
	def delete(KEY: key): Unit
	def lookup(KEY: key,ADR: Ref[address], FOUND: Ref[Boolean]): Unit
}