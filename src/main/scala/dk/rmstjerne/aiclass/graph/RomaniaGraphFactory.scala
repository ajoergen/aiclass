package dk.rmstjerne.aiclass.graph

import dk.rmstjerne.aiclass.search.State
import collection.mutable.HashMap

/**
 * Create a Graph with location of the cities in Romania.
 * User: arj
 * Date: 10/6/11
 * Time: 6:29 PM
 */

object RomaniaGraphFactory {
  private def createGraphOfRomania = {
    val data  = HashMap[Symbol, Map[Symbol, Double]](
      'Arad -> Map('Zerind -> 75.0, 'Sibiu -> 140.0, 'Timisoara -> 118.0),
      'Bucharest -> Map('Urziceni -> 85.0, 'Pitesti -> 101.0, 'Giurgiu -> 90.0, 'Fagaras -> 211.0),
      'Craiova -> Map('Dobreta -> 120.0, 'RimnicuVilcea -> 146.0, 'Pitesti -> 138.0),
      'Dobreta -> Map('Mehadia -> 75.0),
      'Eforie -> Map('Hirsova -> 86.0),
      'Fagaras -> Map('Sibiu -> 99.0),
      'Hirsova -> Map('Urziceni -> 98.0),
      'Iasi -> Map('Vaslui -> 92.0, 'Neamt -> 87.0),
      'Lugoj -> Map('Timisoara->111.0, 'Mehadia ->70.0),
      'Oradea -> Map('Zerind -> 71.0, 'Sibiu -> 151.0),
      'Pitesti -> Map('RimnicuVilcea -> 97.0),
      'RimnicuVilcea -> Map('Sibiu -> 80.0),
      'Urziceni -> Map('Vaslui -> 142.0))
    val distances = HashMap[Symbol, Double](
      'Arad -> 366, 'Bucharest -> 0, 'Craiova -> 160, 'Dobreta -> 242,
      'Eforie -> 161, 'Fagaras -> 176, 'Giurgiu -> 77, 'Hirsova -> 151,
      'Iasi -> 226, 'Lugoj -> 244, 'Mehadia -> 241, 'Neamt -> 234,
      'Oradea -> 380, 'Pitesti -> 100, 'RimnicuVilcea -> 193, 'Sibiu -> 253,
      'Timisoara -> 329, 'Urziceni -> 80, 'Vaslui -> 199, 'Zerind -> 374)
    GraphWithHeuristics[Symbol](data, distances)

  }

  def apply() = createGraphOfRomania
}