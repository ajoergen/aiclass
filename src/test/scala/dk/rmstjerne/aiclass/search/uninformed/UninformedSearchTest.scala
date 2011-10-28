package dk.rmstjerne.aiclass.search.uninformed

import org.scalatest.matchers.{ShouldMatchers, MustMatchers}
import dk.rmstjerne.aiclass.graph.RomaniaGraphFactory
import org.scalatest.{BeforeAndAfterAll, GivenWhenThen, FeatureSpec}
import akka.testkit.{TestActorRef, TestKit}
import akka.util.duration._
import dk.rmstjerne.aiclass.search._

/**
 * Test of uninformed search algorithms.
 * User: arj
 * Date: 10/6/11
 * Time: 8:16 PM
 */

class UninformedSearchTest extends FeatureSpec with GivenWhenThen with MustMatchers with ShouldMatchers with BeforeAndAfterAll with TestKit {
  val problem = GraphProblem('Arad, 'Bucharest, RomaniaGraphFactory())
  val bfsService = TestActorRef(new BreadthFirstSearchService[Symbol]).start()
  val dfsService = TestActorRef(new DepthFirstSearchService[Symbol]).start()

  override def afterAll() {
    bfsService.stop()
    dfsService.stop()
  }

  feature("Uninformed search") {
    info("Searching graph data using uninformed search")
    scenario("Find a path between Arad and Bucharest using breadth first search") {
      given("I have a breadth first search serivce a formulated for finding a path to Bucharest from Arad")

      then("The breadth first search service is running and answering the PerformSearch requests.")
      bfsService.isRunning && bfsService.isDefinedAt(PerformSearch) must  be (true)

      and("Within 200 milliseconds the breadth first service can find the path 'Arad->Sibiu->Faragas->Bucharest'")
      within (200 millis) {
        bfsService ! PerformSearch(problem)
        expectMsg(Goal(List(SearchAction('Sibiu), SearchAction('Fagaras), SearchAction('Bucharest))))
      }
    }

    scenario("Find a path between Arad and Bucharest using depth first search") {
      given("I have a depth first search serivce a formulated for finding a path to Bucharest from Arad")
      then("The depth first service should be running, and be able to receive PerformSearch requests.")
      dfsService.isRunning && dfsService.isDefinedAt(PerformSearch) must be (true)

      and("Within 200 milliseconds the depth forst search agent return the path 'Arad->Timisuara->Lugoj->Mehadi->Dobreta->Craiova->Pitesti->Bucharest'")
      within(200 millis) {
        dfsService ! PerformSearch(problem)
        expectMsg(Goal(List(SearchAction('Timisoara), SearchAction('Lugoj), SearchAction('Mehadia), SearchAction('Dobreta), SearchAction('Craiova), SearchAction('Pitesti), SearchAction('Bucharest))))
      }
    }
  }
}