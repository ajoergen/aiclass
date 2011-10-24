package dk.rmstjerne.aiclass.search.uninformed

import akka.actor.Actor
import dk.rmstjerne.aiclass.search._

/**
 * Breadth first tree search and breadth first graph search
 * User: arj
 * Date: 10/24/11
 * Time: 9:42 PM
 */

/**
 * These search algorithms has to be mixed with the general Search trait.
 */
trait BreadthFirstTreeSearch[S] {this: Actor =>
  protected def performSearch: Receive = {
    case PerformSearch(problem) =>
      self reply TreeSearch(problem, FifoQueue[SearchNode[Any]])
    case _ => self reply Fail("Breadth first tree search received an invalid request")
  }
}

trait BreadthFirstSearch[S] {this: Actor =>
  protected def performSearch: Receive = {
    case PerformSearch(problem) =>
      self reply GraphSearch(problem, FifoQueue[SearchNode[Any]])
    case _ =>
      self reply Fail("Breadth first search received an invalid request")
  }
}
