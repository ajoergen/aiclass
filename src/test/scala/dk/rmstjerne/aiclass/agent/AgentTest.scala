package dk.rmstjerne.aiclass.agent

import org.scalatest.matchers.{MustMatchers, ShouldMatchers}
import akka.actor.Actor._
import akka.actor._
import dk.rmstjerne.aiclass.simplerule.{AndCondition, IsCondition, Rule}
import dk.rmstjerne.aiclass.search.{SearchAction, Goal}
import dk.rmstjerne.aiclass.graph.RomaniaGraphFactory
import akka.testkit.TestKit
import org.scalatest.{BeforeAndAfterAll, GivenWhenThen, FeatureSpec}

class AgentTest extends FeatureSpec with BeforeAndAfterAll with GivenWhenThen with MustMatchers with ShouldMatchers with TestKit {
  var agentRef: ActorRef = testActor

  override def afterAll() {
    stopTestActor
  }

  feature("Test of table driven agent") {
    info("Test of actions given different percepts")
    scenario("The table driven the agent described in Fig. 2.3") {
      given("I can start the table drive agent service.")
      val table = Map("loc_A, Clean" -> "Right", "loc_A, Dirty" -> "Suck",
    "loc_B, Clean" -> "Left", "loc_B, Dirty" -> "Suck",
    "loc_A, Clean, loc_A, Clean" -> "Right", "loc_A, Clean, loc_A, Dirty" -> "Suck",
    "loc_A, Clean, loc_A, Clean, loc_A, Clean" -> "right",
    "loc_A, Clean, loc_A, Clean, loc_A, Dirty" -> "Suck")
      val tableAgent = Actor.remote.actorFor("agent:table_driven", "localhost" ,8270)

      then("The percept (loc_A, Clean) should yield (Right)")
      (tableAgent ? Percept("loc_A, Clean")).as[Action[Option[String]]].getOrElse(throw new Exception("Could not get Action")).value should  be (Some("Right"))
      then("The percept (loc_A, Dirty) should yield (Suck)")
      (tableAgent ? Percept("loc_A, Dirty")).as[Action[Option[String]]].getOrElse(throw new Exception("Could not get Action")).value should  be (Some("Suck"))

      and("I can shutdown the table driven agent service.")
      tableAgent.stop()
    }
  }

  feature("Test of simple reflex based agent.") {
    info("The reflex based agent selects the proper action based on a set of rules.")
    scenario("The simple reflex based vacuum agent.") {
      given("I have a set of rules, and a reflex based agent")
      val ruleSet = {
        def ruleGenerator(key: String, value: String, action: String): List[Rule[Map[String,String],Option[String]]] = {
          List(new Rule(new IsCondition((state: Map[String,String]) => (state.contains(key) && state.get(key) == Some(value))), Some(action)))
        }

        var tmpRules: List[Rule[Map[String,String],Option[String]]] = Nil
        tmpRules ++= ruleGenerator ("status", "Dirty", "Suck")
        tmpRules ++= ruleGenerator ("location", "loc_A", "Right")
        tmpRules ++= ruleGenerator ("location", "loc_B", "Left")
        tmpRules
      }

      def stateExtractor(percept: (String,String)) = Map("location" -> percept._1, "status" -> percept._2)

      //TODO: Consider auto detecting the type parameters using reflection?
      agentRef = actorOf(SimpleReflexAgentService[(String,String), Map[String,String], String](ruleSet,stateExtractor)).start()

      then("Any (Dirty) location should yield (Suck)")
      (agentRef ? Percept(("loc_A", "Dirty"))).as[Action[Option[String]]].getOrElse(throw new Exception("Could not get Action")).value should  be (Some("Suck"))
      (agentRef ? Percept(("loc_B", "Dirty"))).as[Action[Option[String]]].getOrElse(throw new Exception("Could not get Action")).value should  be (Some("Suck"))
      then("An agent at a (Clean) (loc_A) should yield (Right)")
      (agentRef ? Percept(("loc_A", "Clean"))).as[Action[Option[String]]].getOrElse(throw new Exception("Could not get Action")).value should  be (Some("Right"))
      then("An agent at a (Clean) (loc_B) should yield (Left)")
      (agentRef ? Percept(("loc_B", "Clean"))).as[Action[Option[String]]].getOrElse(throw new Exception("Could not get Action")).value should  be (Some("Left"))

      and("I can shut down the agent service.")
      agentRef.stop()
    }
  }

  feature("The model based reflex agent service") {
    info("Model based relfex agent selects the proper action based on a ruleset, and it contains it's current state")
    scenario("The model based reflex agent.") {
      given("I have a set of rules and a model based agent.")
      val ruleSet = {
        // returns EqualCondition
        def eqCondGen(key: String, value: String) =
          new IsCondition( (state: Map[String,String]) => (state.contains(key) && state.get(key) == Some(value)))

        //returns List(Rule(EqualCondition,Action))
        def ruleGen(key: String, value: String, action: String): List[Rule[Map[String,String],Option[String]]] =
          List(new Rule(eqCondGen(key,value),Some(action)))

        var tmpRules: List[Rule[Map[String,String],Option[String]]] = Nil
        tmpRules ++= List[Rule[Map[String,String],Option[String]]](new Rule(new AndCondition
        (eqCondGen("statusloc_A","Clean"),
          eqCondGen("statusloc_B","Clean")),None))
        tmpRules ++= ruleGen("status", "Dirty", "Suck")
        tmpRules ++= ruleGen("location", "loc_A", "Right")
        tmpRules ++= ruleGen("location", "loc_B", "Left")
        tmpRules
      }
      val initialState = Map[String,String]()
      def retrieveState(state: Map[String,String], action: Option[String], percept: (String,String)) = {
        state + ("location" -> percept._1, "status" -> percept._2, "status" + percept._1 -> percept._2)
      }
      agentRef = actorOf(ModelBasedReflexAgentService[(String,String),Map[String,String], String](initialState, ruleSet, retrieveState)).start()

      then("Any (Dirty) location should yield (Suck)")
      (agentRef ? Percept(("loc_A", "Dirty"))).as[Action[Option[String]]].getOrElse(throw new Exception("Could not get Action")).value should  be (Some("Suck"))
      (agentRef ? Percept(("loc_B", "Dirty"))).as[Action[Option[String]]].getOrElse(throw new Exception("Could not get Action")).value should  be (Some("Suck"))
      then("Two subsequent calls to locations with status (Clean) results in no action.")
      (agentRef ? Percept(("loc_A", "Clean"))).as[Action[Option[String]]].getOrElse(throw new Exception("Could not get Action")).value should  be (Some("Right"))
      (agentRef ? Percept(("loc_B", "Clean"))).as[Action[Option[String]]].getOrElse(throw new Exception("Could not get Action")).value should  be (None)
      and("I can stop the model based agent service")
      agentRef.stop()
    }
  }

  feature("A* search agnet service integration test") {
    info("Searching graph data using the A* search agent service.")
    scenario("Find a path between Arad and Bucharest using A* search") {
      given("I have a graph over Romania, and has it formulated as a graph problem")
      val graph = RomaniaGraphFactory()
      agentRef = actorOf(new AStarSearchAgentService[Symbol](graph)).start()

      when("I seach the graph for a path between 'Arad and 'Bucharest")
      val result = (agentRef ? Solve('Arad, 'Bucharest)).as[Goal[List[SearchAction[Symbol]]]].getOrElse(throw new Exception("Unable to get actions."))

      then("The result must be the path 'Arad -> 'Sibiu -> 'RimnicuValcea -> 'Pitesti -> 'Bucharest")
      result must be (Goal(List(SearchAction('Sibiu), SearchAction('RimnicuVilcea), SearchAction('Pitesti), SearchAction('Bucharest))))

      and("I can shut down the agent.")
      agentRef stop()
    }
  }
}