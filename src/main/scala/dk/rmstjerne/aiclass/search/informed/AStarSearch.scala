package dk.rmstjerne.aiclass.search.informed

import akka.actor.Actor
import dk.rmstjerne.aiclass.search._

/**
 * A* graph search algorithm
 * User: arj
 * Date: 10/24/11
 * Time: 9:52 PM
 */

trait AStarSearch[+S] {this: Actor =>
  protected def performSearch: Receive  = {
    case PerformSearch(problem) =>
      self reply GraphSearch(problem, PrioritySearchQueue[SearchNode[Any]](
        new Ordering[SearchNode[Any]] {
          def compare(a: SearchNode[Any], b: SearchNode[Any]) = {
            heuristic(b, problem).compare(heuristic(a,problem))
          }
      }))
    case _ => self reply (Fail("The given message was not a search problem"))
  }

  private def heuristic[S](node: SearchNode[S], problem: Problem[S]) = {
    node.pathCost + problem.estimatedCostToGoal(node.state)
  }
}
