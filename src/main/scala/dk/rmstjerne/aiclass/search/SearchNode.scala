package dk.rmstjerne.aiclass.search

/**
 * The node abstraction described in Fig. 3.10
 * S - State
 * A - Action
 * C - Cost
 *
 * User: arj
 * Date: 10/6/11
 * Time: 8:43 AM
 */

class SearchNode[S<:Any](val state: S, val parent: Option[SearchNode[S]], val action: Option[SearchAction[S]],
                           val depth: Int,  val pathCost: Double) {
  def actionPath: List[SearchAction[S]] = {
    def buildPath(node: SearchNode[S], listOfActions: List[SearchAction[S]]): List[SearchAction[S]] = {
      node.parent match {
        case None => listOfActions
        case Some(parentNode) => buildPath(parentNode, node.action.get :: listOfActions)
      }
    }
    buildPath(this, Nil)
  }

  override def toString = state.toString + " -> " + pathCost
}

object SearchNode {
  def apply[S](state: S) = new SearchNode[S](state, None, None, 0, 0.0)
  def apply[S](state: S,  parent: Option[SearchNode[S]], action: Option[SearchAction[S]], depth: Int, pathCost: Double) = {
    new SearchNode[S](state, parent, action, depth, pathCost)
  }
}