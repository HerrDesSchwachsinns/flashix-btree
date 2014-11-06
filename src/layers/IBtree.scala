package layers

import datatypes.key
import misc.address
import helpers.scala.Ref

trait IBtree {
  /**
   * insert address with key into Btree
   */
  def insert(KEY: key, ADR: address): Unit
  /**
   * delete address with key from Btree
   */
  def delete(KEY: key): Unit
  /**
   * find address with key in Btree FOUND is set to true if key was found and
   * ADR is set to found address if any
   */
  def lookup(KEY: key, ADR: Ref[address], FOUND: Ref[Boolean]): Unit
}