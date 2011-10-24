package dk.rmstjerne.aiclass.search

import collection.mutable.HashSet
import akka.actor.Actor
import akka.config.Supervision._
import informed.AStarSearch
import uninformed.{DepthFirstSearch, DepthFirstTreeSearch, BreadthFirstSearch, BreadthFirstTreeSearch}

/**
 * Collection of general search algorithms.
 * User: arj
 * Date: 10/6/11
 * Time: 11:27 AM
 */

sealed trait SearchResult[A]
case class Goal[A](result: A) extends SearchResult[A]
case class Fail[A](result: A) extends SearchResult[A]
case class CutOff[A](result: A) extends SearchResult[A]

sealed trait SearchRequest[S]
case class PerformSearch[S](problem: Problem[S]) extends SearchRequest[S]


trait Search extends Actor {
  self.lifeCycle = Permanent
  def receive = performSearch

  protected def performSearch: Receive
}

class BreadthFirstTreeSearchService[S] extends  Search with BreadthFirstTreeSearch[S]

class BreadthFirstSearchService[S] extends Search with BreadthFirstSearch[S]

class DepthFirstTreeSearchService[S] extends Search with DepthFirstTreeSearch[S]

class DepthFirstSearchService[S] extends Search with DepthFirstSearch[S]

class AStarSearchService[S] extends Search with AStarSearch[S]


object TreeSearch {
  def apply[S](problem: Problem[S], frontier: SearchQueue[SearchNode[S]]): SearchResult[List[SearchAction[S]]] = {
    def createNewNode(problem: Problem[S], node: SearchNode[S], action: SearchAction[S]): SearchNode[S] = {
      val newState  = problem.result(node.state, action)
      SearchNode(newState, Some(node), Some(action), node.depth + 1, node.pathCost + problem.stepCost(node.state, action, newState))
    }

    def treeSearch(frontier: SearchQueue[SearchNode[S]]): SearchResult[List[SearchAction[S]]] = {
      frontier.pop match {
        case None => Fail(List[SearchAction[S]]()) // Fail to find a solution yields the empty set.
        case Some(node) if (problem.isRequestedState(node.state)) => Goal(node.actionPath)
        case Some(node) =>
          problem.actions(node.state) foreach  (a => frontier.insert(createNewNode(problem, node, a)))
          treeSearch(frontier)
      }
    }
    treeSearch(frontier.insert(SearchNode(problem.initialState)))
  }
}

object GraphSearch {
  def apply[S<:Any](problem: Problem[S], frontier: SearchQueue[SearchNode[S]]):SearchResult[List[SearchAction[S]]] = {

    def createNewNode(problem: Problem[S], node: SearchNode[S], action: SearchAction[S]): SearchNode[S] = {
      val newState  = problem.result(node.state, action)
      SearchNode(newState, Some(node), Some(action), node.depth + 1, node.pathCost + problem.stepCost(node.state, action, newState))
    }

    def graphSearch(frontier: SearchQueue[SearchNode[S]], exploredSet: collection.mutable.Set[S]):SearchResult[List[SearchAction[S]]] = {
      frontier.pop match {
        case None =>Fail(List[SearchAction[S]]()) // The solution is the empty set.
        case Some(node) if (problem.isRequestedState(node.state)) => Goal(node.actionPath)
        case Some(node) =>
          if (!exploredSet(node.state)) {
            exploredSet += node.state
            problem.actions(node.state) foreach(a => frontier.insert(createNewNode(problem, node, a)))
            graphSearch(frontier, exploredSet)
          } else {
            graphSearch(frontier, exploredSet)
          }
      }
    }
    // HashSet gives constant time search and insert.
    graphSearch(frontier.insert(SearchNode(problem.initialState)), HashSet[S]())
  }
}