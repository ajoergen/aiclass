package dk.rmstjerne.aiclass.agent

import akka.actor._
import akka.actor.Actor._
import akka.stm._
import akka.config.Supervision._
import dk.rmstjerne.aiclass.search._
import dk.rmstjerne.aiclass.tree.Tree
import dk.rmstjerne.aiclass.graph.Graph

/**
 *  Class hierarchy is as follows.
 *
 *  AIObject
 *    - Agent
 *      -- TableDrivenAgent
 *      -- SimpleSearchAgent
 */

/**
 * P - Percept
 * A - Action
 */
sealed trait AIEvent

case class Percept[P](value: P) extends AIEvent

case class Action[A](value: A) extends AIEvent

case class InitTableStorage[P<:Any,A<:Any](table: Map[P,A]) extends AIEvent

case class Solve[S<:Any](startState: S,  goalState: S) extends AIEvent


/**
 * Object abstraction that appears in an artificial intelligence environment.
 */
trait AIObject {
  var location = None
}

/**
 * The basic agent actor. It declares the alive and performance fields. The receive method just forwards to the agent
 * program functionality.
 */
trait Agent extends Actor with AIObject {
  var alive = true

  def receive = program

  protected def program: Receive
}

/**
 * Table driven agent. It receives a percept and based on the table provided to the constructor, selects the appropriate
 * action. This agent is of cause only suitable for limited size tables.
 */
trait TableDrivenAgent { this: Actor =>
  self.faultHandler = OneForOneStrategy(List(classOf[Throwable]), 5, 5000)
  protected val table: ActorRef // Mix in some table storage factory to get the table.

  protected def program: Receive = {
    case msg @ Percept(_) => table forward msg // Just forward the message to the table implementation.
  }
}

/**
 * The table storage abstraction actor
 */
trait TableStorage[P,A] extends Actor

/**
 * State memory implmentation of the @see "TableStorage" abstraction. This will utilises shared transactional memory
 * from the akka framework. State the event that requests should be incomming before the lookup table is proberly
 * initialized. Initialization should therefore be atomic.
 */
class MemoryBasedTableStorage[P,A] extends TableStorage[P,A] {
  self.lifeCycle = Permanent
  private[this] var table = TransactionalMap[Any,Any]()


  def receive = {
    case InitTableStorage(initTable) =>  {
      atomic { table ++= initTable }
    }

    case Percept(percept: String) => {
      self reply Action(Some(table(percept)))
    }
  }
}

/**
 * Factory trait for the in memory table storage.
 */
trait MemoryBasedTableStorageFactory[P,A] { this: Actor =>
  val table = actorOf(new MemoryBasedTableStorage[P, A]).start()
  self.link(table)
}

/**
 * The table driven agent service, that implements the table driven agent described in aima Fig. 2.7. This is mixed in with
 * a memory based table storage. Switching to a say database base table storage, should just be a question of changing the
 * mixin trait.
 *
 * The constructor is private so the agen can only be instanciated from the companion object.
 */
class TableDrivenAgentService[P,A] private (data: Map[P, A]) extends Agent with TableDrivenAgent with MemoryBasedTableStorageFactory[P,A] {
  override def preStart() {
    table ! InitTableStorage(data)
  }

  override def postStop() {
    self.unlink(table)
    table.stop()
  }
}

/**
 * The companion object to the table driven agent service. Use this to instantiate the service.
 */
object TableDrivenAgentService {
  def apply[P,A](data: Map[P,A]) = new TableDrivenAgentService[P,A](data)
}


trait SimpleSearchAgent[S] {this: Actor =>
  self.faultHandler = OneForOneStrategy(List(classOf[Throwable]), 5, 5000)
  val searchService: ActorRef

  protected def formulateProblem(startState: S, goalState: S): Problem[S]

  protected def program: Receive = {
    case Solve(startState: S, goalState: S) =>
      searchService  forward PerformSearch(formulateProblem(startState, goalState))
  }

  override def postStop() {
    self.unlink(searchService)
    searchService.stop()
  }
}

trait BreadthFirstTreeSearchFactory[S] {this: Actor =>
  val searchService = actorOf(new BreadthFirstTreeSearchService[S]).start()
  self.link(searchService)
}

class BreadthFirstTreeSearchAgentService[S<:Any](tree: Tree[S]) extends Agent with SimpleSearchAgent[S] with BreadthFirstTreeSearchFactory[S] {
  protected def formulateProblem(startState: S, goalState: S) = TreeProblem[S](startState, goalState, tree)
}

trait BreadthFirstSearchFactory[S] {this: Actor =>
  val searchService = actorOf(new BreadthFirstSearchService[S]).start()
  self.link(searchService)
}

class BreadthFirstSearchAgentService[S](graph: Graph[S]) extends Agent with SimpleSearchAgent[S] with BreadthFirstSearchFactory[S] {
  protected def formulateProblem(startState: S, goalState: S) = GraphProblem[S](startState, goalState, graph)
}

trait DepthFirstTreeSearchFactory[S] {this: Actor =>
  val searchService = actorOf(new DepthFirstTreeSearchService[S]).start()
  self.link(searchService)
}

class DepthFirstTreeSearchAgentService[S](tree: Tree[S]) extends Agent with SimpleSearchAgent[S] with DepthFirstTreeSearchFactory[S] {
  protected def formulateProblem(startState: S, goalState: S) = TreeProblem[S](startState, goalState, tree)
}

trait DepthFirstSearchFactory[S] {this: Actor =>
  val searchService = actorOf(new DepthFirstSearchService[S]).start()
  self.link(searchService)
}

class DepthFirstSearchAgentService[S](graph: Graph[S]) extends Agent with SimpleSearchAgent[S] with DepthFirstSearchFactory[S] {
  protected def formulateProblem(startState: S,  goalState: S) = GraphProblem(startState, goalState, graph)
}

trait AStarSearchFactory[S] {this: Actor =>
  val searchService = actorOf(new AStarSearchService[S]).start()
  self.link(searchService)
}

class AStarSearchAgentService[S](graph: Graph[S]) extends Agent with SimpleSearchAgent[S] with AStarSearchFactory[S] {
  protected def formulateProblem(startState: S,  goalState: S) = GraphProblem(startState, goalState, graph)
}