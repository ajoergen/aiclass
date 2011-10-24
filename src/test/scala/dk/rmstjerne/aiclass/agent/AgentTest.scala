package dk.rmstjerne.aiclass.agent

import org.scalatest.matchers.{MustMatchers, ShouldMatchers}
import akka.actor.Actor._
import akka.actor._
import dk.rmstjerne.aiclass.search.{SearchAction, Goal}
import dk.rmstjerne.aiclass.graph.RomaniaGraphFactory
import akka.testkit.TestKit
import org.scalatest.{BeforeAndAfterAll, GivenWhenThen, FeatureSpec}

class AgentTest extends FeatureSpec with BeforeAndAfterAll with GivenWhenThen with MustMatchers with ShouldMatchers with TestKit {
  feature("Test of table driven agent") {
    info("Test of actions given different percepts")
    scenario("The table driven the agent described in Fig. 2.3") {
      given("I can start the table drive agent service.")
      val table = Map("loc_A, Clean" -> "Right", "loc_A, Dirty" -> "Suck",
    "loc_B, Clean" -> "Left", "loc_B, Dirty" -> "Suck",
    "loc_A, Clean, loc_A, Clean" -> "Right", "loc_A, Clean, loc_A, Dirty" -> "Suck",
    "loc_A, Clean, loc_A, Clean, loc_A, Clean" -> "right",
    "loc_A, Clean, loc_A, Clean, loc_A, Dirty" -> "Suck")
      val tableAgent = actorOf(TableDrivenAgentService(table)).start()

      then("The percept (loc_A, Clean) should yield (Right)")
      (tableAgent ? Percept("loc_A, Clean")).as[Action[Option[String]]].getOrElse(throw new Exception("Could not get Action")).value should  be (Some("Right"))
      then("The percept (loc_A, Dirty) should yield (Suck)")
      (tableAgent ? Percept("loc_A, Dirty")).as[Action[Option[String]]].getOrElse(throw new Exception("Could not get Action")).value should  be (Some("Suck"))

      and("I can shutdown the table driven agent service.")
      tableAgent.stop()
    }
  }

  feature("A* search agnet service integration test") {
    info("Searching graph data using the A* search agent service.")
    scenario("Find a path between Arad and Bucharest using A* search") {
      given("I have a graph over Romania, and has it formulated as a graph problem")
      val graph = RomaniaGraphFactory()
      val agentRef = actorOf(new AStarSearchAgentService[Symbol](graph)).start()

      when("I seach the graph for a path between 'Arad and 'Bucharest")
      val result = (agentRef ? Solve('Arad, 'Bucharest)).as[Goal[List[SearchAction[Symbol]]]].getOrElse(throw new Exception("Unable to get actions."))

      then("The result must be the path 'Arad -> 'Sibiu -> 'RimnicuValcea -> 'Pitesti -> 'Bucharest")
      result must be (Goal(List(SearchAction('Sibiu), SearchAction('RimnicuVilcea), SearchAction('Pitesti), SearchAction('Bucharest))))

      and("I can shut down the agent.")
      agentRef stop()
    }
  }
}