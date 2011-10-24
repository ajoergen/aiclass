package dk.rmstjerne.aiclass.search

import dk.rmstjerne.aiclass.tree.Tree
import dk.rmstjerne.aiclass.graph.{GraphWithHeuristics, Graph}

/**
 * A generic Problem abstraction.
 *
 * S - State type
 * A - Action type
 *
 * User: arj
 * Date: 10/5/11
 * Time: 9:55 PM
 */
case class State[A](state: A)
case class SearchAction[A](destinationState: A)


abstract class Problem[S<:Any] {
  val initialState: S
  def isRequestedState(state: S): Boolean

  def actions(state: S): List[SearchAction[S]]
  def result(state: S,  action: SearchAction[S]): S
  def stepCost(from: S,  action: SearchAction[S], to: S): Double

  def estimatedCostToGoal(from: S): Double
}

class GraphProblem[S](val startingState: S, val goal: S, graph: Graph[S]) extends Problem[S] {
  val initialState = startingState

  def isRequestedState(state: S) = state == goal

  def actions(state: S) = {
    graph.getAdjacentNodes(state).keys.map(in => SearchAction(in)).toList
  }

  def result(state: S, action: SearchAction[S]) = action.destinationState

  def stepCost(from: S, action: SearchAction[S], to: S) = graph.getAdjacentNodes(from)(to)

  def estimatedCostToGoal(from: S): Double = {
    if (graph.isInstanceOf[GraphWithHeuristics[S]]) {
      graph.asInstanceOf[GraphWithHeuristics[S]].getHeuristic(from)
    } else {
      Double.MaxValue
    }
  }
}

class TreeProblem[S](val startingState: S, val goal: S, tree: Tree[S]) extends Problem[S]{
  val initialState = startingState

  def isRequestedState(state: S) = state == goal

  def actions(state: S) = tree.childNodes(state).map(in => SearchAction(in))

  def result(state: S, action: SearchAction[S]) = action.destinationState

  def stepCost(from: S, action: SearchAction[S], to: S) = Double.MaxValue

  def estimatedCostToGoal(from: S) = Double.MaxValue
}

object GraphProblem {
  def apply[S](startingState: S, goal: S, graph: Graph[S]) =
    new GraphProblem[S](startingState, goal, graph)
}

object TreeProblem {
  def apply[S](startingState: S,  goal: S, tree: Tree[S]) =
    new TreeProblem[S](startingState, goal, tree)
}
