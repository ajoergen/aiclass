package dk.rmstjerne.aiclass.learning

import akka.actor.Actor
import collection.mutable.{HashSet, HashMap, Map}
import collection.Iterable

/**
 * Code for machine learning
 * User: arj
 * Date: 10/29/11
 * Time: 8:52 AM
 */

sealed trait LearningRequest
case class Train(dataSet: DataSet)
case class Predict(query: String)
case class Prediction(prediction: (Symbol, Double))

trait Learner extends Actor {
  /**
   * Used for training the software, given the specified data set.
   */
  def train(dataSet: DataSet)

  /**
   * Predict the class of the specified query value. The function returns a tuple containing the predicted class, and
   * the calculated weight given as a real number between 0 and 1.
   */
  def predict(query: String): (Symbol, Double)

  def receive = {
    case Train(dataSet: DataSet) => train(dataSet)
    case Predict(query: String) => self reply (Prediction(predict(query)))
    case _ => throw new Exception("Learner received an unknown request")
  }
}

/*
 * The code is a little messy and needs some cleaning up, but did pass the quiz's of unit 5 :-)
 */
class NaiveBayesLearner(valueExtractor: (String => List[String]), val k: Double = 0) extends Learner {
  /**
   * BayesNet maps class to (P(class), Bag of Words) - We use lazy evaluation of the values P(value|class), so
   * it's not included in the data structure.
   */
  val bayesNet = HashMap[Symbol, (Double, Map[String, Int])]()
  val valueSet = HashSet[String]()

  private def extractValues(list: List[String]) = {
    var result = HashMap[String, Int]()
    list.foreach(valueExtractor(_).foreach(v => {
      valueSet += v
      if (!result.keySet.contains(v))
        result += (v -> 0)
      result += (v -> (result(v) + 1))
    }))
    result
  }

  def train(dataSet: DataSet) {
    for (classValue <- dataSet.classes()) {
      val probClass = probability(dataSet.valueSetSizeForClass(classValue), dataSet.totalValuesSetSize, dataSet.classes().size)
      bayesNet += (classValue -> (probClass, extractValues(dataSet.values(classValue))))
    }
  }

  /**
   * Calculates the probabilities P(value|class) for all classes.
   */
  private def deriveProbabilitiesForValue(value: String, classes: Iterable[Symbol]) = {
    var result = HashMap[Symbol, Double]()
    classes.foreach(c => {
      val valueCountForClass = bayesNet(c)._2.getOrElse(value, 0)
      val totalCountForClass = bayesNet(c)._2.values.sum
      result += (c -> probability(valueCountForClass, totalCountForClass, valueSet.size))
    })
    result
  }

  private def createProbabilityMap(value: String) = {
    val probMap = HashMap[String, Map[Symbol, Double]]()
    valueExtractor(value).foreach(v => probMap += (v -> deriveProbabilitiesForValue (v, bayesNet.keys)))
    probMap
  }

  def predict(query: String) = {
    val probabilityMap = createProbabilityMap(query)
    var probabiliesOfClassGivenQuery = List[(Symbol, Double)]()
    bayesNet.keys.foreach(c => {
      val probabilityGivenClass = valueExtractor(query).foldLeft(bayesNet(c)._1)((a,b) => a * probabilityMap(b)(c))
      probabiliesOfClassGivenQuery = (c, probabilityGivenClass)::probabiliesOfClassGivenQuery
    })
    val totalProbabilityQuery = probabiliesOfClassGivenQuery.foldLeft(0.0)((a, b) => a + b._2)
    // Normalize the problabilities with the total
    probabiliesOfClassGivenQuery = for ((classVal, prob) <- probabiliesOfClassGivenQuery) yield (classVal, prob/totalProbabilityQuery)
    probabiliesOfClassGivenQuery.foldLeft((probabiliesOfClassGivenQuery(0)._1, Double.MinValue))((a,b) => if (b._2 > a._2) b else a)
  }

  private def probability(valueCountGiven: Int, totalCountGiven: Int, totalOutcomes: Int) = {
    (valueCountGiven + k)/(totalCountGiven + k*totalOutcomes)
  }
}


