package dk.rmstjerne.aiclass.search

import collection.mutable.{PriorityQueue, HashSet, Stack, Queue}

trait SearchQueue[A] {
  val valuesSet = HashSet[A]()
  def isEmpty: Boolean
  def insert(values: A*): SearchQueue[A]
  def pop: Option[A]

  protected def contains(value: A) = valuesSet(value)
}
/**
 *
 * User: arj
 * Date: 10/6/11
 * Time: 10:40 AM
 */

class FifoQueue[A] extends SearchQueue[A] {
  val queue = Queue[A]()

  def isEmpty = queue.isEmpty

  def insert(values: A*) = {
    values.foreach(value => if (!contains(value)) insertInternal(value))
    this
  }

  def insertInternal(value: A) = {
    queue += value
    valuesSet += value
  }

  def pop = {
    val result = queue.dequeue()
    valuesSet -= result
    Some(result)
  }
}

object FifoQueue {
  def apply[A] = new FifoQueue[A]()
}

class LifoQueue[A] extends SearchQueue[A] {
  val queue = Stack[A]()

  def isEmpty = queue.isEmpty

  def insert(values: A*) = {
    values.foreach(value => if (!contains(value)) insertInternal(value))
    this
  }

  private def insertInternal(value: A) = {
    queue push(value)
    valuesSet += value
  }

  def pop = {
    val result = queue.pop()
    valuesSet -= result
    Some(result)
  }
}

object LifoQueue {
  def apply[A] = new LifoQueue[A]()
  def apply[A](elems: A) = {
    val q = new LifoQueue[A]()
    q.insert(elems)
    q
  }
}

class PrioritySearchQueue[A](ordering: Ordering[A]) extends SearchQueue[A] {
  val queue = new PriorityQueue[A]()(ordering)
  // We maintain a set of the values in order to be able to perform contains in O(1)
  val valueSet = new HashSet[A]()


  def isEmpty = queue.isEmpty

  def insert(values: A*) = {
    values foreach(queue += _)
    valueSet ++= values
    this
  }

  def pop = {
    if (queue.isEmpty) {
      None
    }
    else {
      val result = queue.dequeue()
      valueSet -= result
      Some(result)
    }
  }

  override def toString() = queue.toString
}

object PrioritySearchQueue {
  def apply[A](ordering: Ordering[A]) = new PrioritySearchQueue[A](ordering)
}