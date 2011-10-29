package dk.rmstjerne.aiclass.learning

import collection.mutable.{HashMap}

/**
 * A data set wrapper used by learning algorithms.
 * User: arj
 * Date: 10/28/11
 * Time: 11:20 PM
 */

class DataSet(val dictionary: HashMap[Symbol, List[String]]) {
  def classes() = dictionary.keys
  def values(classVal: Symbol) = dictionary(classVal)
  def valueSetSizeForClass(classVal: Symbol) = dictionary(classVal).size
  def totalValuesSetSize = {
    var total = 0
    classes().foreach(v => total += valueSetSizeForClass(v))
    total
  }
}

object DataSet {
  def apply(dict: HashMap[Symbol,  List[String]]) = new DataSet(dict)
}