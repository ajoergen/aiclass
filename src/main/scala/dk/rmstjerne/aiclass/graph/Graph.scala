package dk.rmstjerne.aiclass.graph

import collection.mutable.HashMap

/**
 * A graph abstraction. The graph is implemented as an adjecency list.
 * User: arj
 * Date: 9/28/11
 * Time: 9:36 AM
 */
class Graph[V](val graph: HashMap[V, Map[V, Double]] = HashMap[V,Map[V, Double]](), directed: Boolean = false) {
  if (!directed) {
    vertices foreach(a => graph(a).foreach(b => connectInternal(b._1, a, b._2)))
  }

  def connect(a: V, b: V, weight: Double) {
    connectInternal(a, b, weight)
    if (!directed) connectInternal(b, a, weight)
  }

  def vertices() = graph.keys

  def getAdjacentNodes(a: V) = graph(a)

  private def connectInternal(a: V, b: V, weight: Double) {
    if (!graph.contains(a))
      graph += (a -> Map(b -> weight))
    else
      graph(a) += (b -> weight)
  }
}

object Graph {
  def apply[V](data: HashMap[V, Map[V,Double]], directed: Boolean = false) = new Graph[V](data)
}


trait Heuristic[V] {
  val distanceMap: HashMap[V, Double]

  def addHeuristic(state: V, distance: Double) = {
    distanceMap += (state -> distance)
  }

  def getHeuristic(state: V) = distanceMap(state)
}

class GraphWithHeuristics[V](override val graph: HashMap[V, Map[V, Double]] = HashMap[V,Map[V, Double]](),
                              val directed: Boolean = false, val distances: HashMap[V, Double]) extends Graph[V](graph, directed)
                            with Heuristic[V] {
  val distanceMap = distances
}

object GraphWithHeuristics {
  def apply[V](data: HashMap[V, Map[V, Double]], distances: HashMap[V, Double], directed: Boolean = false) = {
    new GraphWithHeuristics[V](data, directed, distances)
  }
}