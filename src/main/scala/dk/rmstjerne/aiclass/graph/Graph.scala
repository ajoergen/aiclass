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

class UnweigtedGraph[V](val graph: HashMap[V, List[V]] = HashMap[V,  List[V]](), directed: Boolean = false) {
  if (!directed)
    vertices foreach(a => graph(a).foreach(b => connectInternal(b, a)))

  def connect(a: V, b: V) {
    connectInternal(a, b)
    if (!directed) connectInternal(b,a)
  }
  def vertices() = graph.keys

  def getAdjacentNodes(a: V) = graph(a)

  private def connectInternal(nodeA: V, nodeB: V) {
    if (graph.contains(nodeA))
      graph += (nodeA -> (graph(nodeA):::List(nodeB)))
    else
      graph += (nodeA -> List(nodeB))

  }
}

object UnweightedGraph {
  def apply[V](data: HashMap[V, List[V]], directed: Boolean = false) = new UnweigtedGraph[V](data, directed)
}

//trait Location[V] extends Heuristic[V] {
//  val distanceMap: HashMap[V, (Double,Double)]
//
//  def addHeuristic(location: V, position: (Double, Double)) =  {
//    distanceMap += (location -> position)
//  }
//
//  def getStraightLineDistance(source: V, destination: V) = {
//    val sourceLocation = distanceMap(source)
//    val destinationLocation = distanceMap(destination)
//    math.sqrt(math.pow(destinationLocation._1-sourceLocation._1, 2) +
//      math.pow (destinationLocation._2 - sourceLocation._2, 2))
//  }
//}

trait Heuristic[V] {
  val distanceMap: HashMap[V, Double]

  def addHeuristic(state: V, distance: Double) = {
    distanceMap += (state -> distance)
  }

  def getHeuristic(state: V) = distanceMap(state)
}

//class GraphWithLocation[V](override val graph: HashMap[V, Map[V, Double]] = HashMap[V,Map[V, Double]](),
//                           val directed: Boolean = false,
//                           locations: HashMap[V, (Double,  Double)]) extends Graph[V](graph, directed)
//                           with Location[V] {
//  val distanceMap = locations
//}
//
//object GraphWithLocation {
//  def apply[V](graph: HashMap[V, Map[V, Double]], locations: HashMap[V, (Double,  Double)], directed: Boolean = false) = {
//    new GraphWithLocation[V](graph, directed, locations)
//  }
//}

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