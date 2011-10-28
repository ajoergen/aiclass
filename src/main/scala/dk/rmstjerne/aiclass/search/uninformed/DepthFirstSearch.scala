package dk.rmstjerne.aiclass.search.uninformed

import akka.actor.Actor
import dk.rmstjerne.aiclass.search._

/**
 * Depth first search algorithm
 * User: arj
 * Date: 10/28/11
 * Time: 5:02 PM
 */

trait DepthFirstTreeSearch[S] {this: Actor =>
  protected def performSearch: Receive = {
    case PerformSearch(problem) =>
      self reply TreeSearch(problem, LifoQueue[SearchNode[Any]])
    case _ =>
      self reply Fail("Depth first tree search received a invalid request.")
  }
}

trait DepthFirstSearch[S] {this: Actor =>
  protected def performSearch: Receive = {
    case PerformSearch(problem) =>
      self reply GraphSearch(problem, LifoQueue[SearchNode[Any]])
    case _ =>
      self reply  Fail("Depth first search received an invalid request")
  }
}
