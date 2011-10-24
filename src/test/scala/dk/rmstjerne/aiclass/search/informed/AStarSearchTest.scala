package dk.rmstjerne.aiclass.search.informedsearch

import org.scalatest.matchers.{ShouldMatchers, MustMatchers}
import akka.testkit.{TestActorRef, TestKit}
import akka.util.duration._
import dk.rmstjerne.aiclass.graph.RomaniaGraphFactory
import dk.rmstjerne.aiclass.search._
import org.scalatest.{BeforeAndAfterAll, GivenWhenThen, FeatureSpec}

/**
 * Unit test of the search A* search agent service using the akka-testkit module.
 * User: arj
 * Date: 10/18/11
 * Time: 9:42 PM
 */

class AStarSearchTest extends FeatureSpec with BeforeAndAfterAll with GivenWhenThen with MustMatchers with ShouldMatchers with TestKit {
  val problem = GraphProblem[Symbol]('Arad, 'Bucharest, RomaniaGraphFactory())
  val aStar = TestActorRef(new AStarSearchService[Symbol]).start()

  override def afterAll() {
    aStar.stop()
  }

  feature("A* search algorithm unit test.") {
    info("Testing the A* algorithm in isolation")
    scenario("I am located in Arad and want to go to Bucharest. What is the shortest possible route?") {
      given("I define a problem giving Arad as starting point and Bucharest as goal, and the graph over routes in Romania")
      then("The actor should be running and able to receive PerformSearch requests")
      aStar.isRunning && aStar.isDefinedAt(PerformSearch) must be (true)
      and("The correct anwer should be available within 100 milliseconds")
      within(100 millis) {
        aStar ! PerformSearch(problem)
        expectMsg(Goal(List(SearchAction('Sibiu), SearchAction('RimnicuVilcea), SearchAction('Pitesti), SearchAction('Bucharest))))
      }
    }
  }
}