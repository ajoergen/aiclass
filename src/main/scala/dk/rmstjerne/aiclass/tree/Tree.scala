package dk.rmstjerne.aiclass.tree

import collection.mutable.HashMap

/**
 * A generic Tree abstraction.
 * User: arj
 * Date: 10/15/11
 * Time: 12:33 PM
 * SearchAction change this template use File | Settings | File Templates.
 */

class Tree[V](val tree: HashMap[V,  List[V]] = HashMap[V, List[V]]()) {
  def connect(a: V,  b: V) {
    if (tree.contains(a))
      tree(a) ::: List(b)
    else
      tree += (a -> List(b))
  }

  def nodes = tree.keys

  def childNodes(a: V) = tree(a)
}

object Tree {
  def apply[V] = new Tree[V]()
  def apply[V](tree: HashMap[V, List[V]]) = new Tree[V](tree)
}