package dk.rmstjerne.aiclass.learning

import org.scalatest.{GivenWhenThen, BeforeAndAfterAll, FeatureSpec}
import org.scalatest.matchers.{ShouldMatchers, MustMatchers}
import collection.mutable.HashMap
import akka.testkit.{TestActorRef, TestKit}

/**
 * Test of the Naive Bayes net learner.
 * User: arj
 * Date: 10/29/11
 * Time: 1:19 PM
 */

class BayesNetLearnerTest extends FeatureSpec with BeforeAndAfterAll with GivenWhenThen with MustMatchers with ShouldMatchers with TestKit {
  val MovieAndSongDataSet = MovieAndSongFactory()
  val spamAndHamDataSet = SpamAndHamFactory()
  val movieAndSongLearnerRef = TestActorRef(new NaiveBayesLearner(wordsFromTitleExtractor, 1.0)).start()
  val spamAndHamLearnerRef = TestActorRef(new NaiveBayesLearner(wordsFromTitleExtractor))
  val spamAndHamLearnerRefWithLaplaceRef = TestActorRef(new NaiveBayesLearner(wordsFromTitleExtractor, 1.0)).start()


  def wordsFromTitleExtractor(title: String) = title.split(" ").toList

  override def beforeAll() {
    spamAndHamLearnerRefWithLaplaceRef ! Train(spamAndHamDataSet)
  }

  override def afterAll() {
    movieAndSongLearnerRef.stop()
    spamAndHamLearnerRefWithLaplaceRef.stop()
  }

  feature("I am able to train the Bayes learner using my dataset.") {
    info("Training is done by accessing the train method directly")
    scenario("Training the BayesLearner should yield a non empty bayes net.") {
      given("I can extract the actor from the ref")
      val learner = movieAndSongLearnerRef.underlyingActor

      then("I can call the train method on the actor.")
      learner.train(MovieAndSongDataSet)

      and("The internal bayes net should be non empty")
      learner.bayesNet.isEmpty must not be(true)
    }
  }

  feature("The Bayes net can predict the correct classification of a known title") {
    info("Given a query no an known title will give the correct classification")
    scenario("With a trained bayes net I can predict the class of a known title") {
      given("I send the query \"Pretty Woman\" to the network")
      val result = (movieAndSongLearnerRef ? Predict("Pretty Woman")).as[Prediction].getOrElse(throw new Exception("Unable to get prediction"))

      then("The result should be movie")
      val (titleClass, probability) = result.prediction
      (probability == 0.8571428571428572) && (titleClass == 'Movie) should be (true)
    }
  }
  feature("I can send a Precict query, and get the most probable class") {
    info("Given a query on an unknown title give the most probable class")
    scenario("With a trained BayesNet I can predict the class of an unknwon title") {
      then("Sending a Predict request for \"Perfect Storm\" should give a result")
      val result = (movieAndSongLearnerRef ? Predict("Perfect Storm")).as[Prediction].getOrElse(throw new Exception("Unable to get prediction"))
      and("The resulting class will be a 'Song")
      val (titleClass, probability) = result.prediction
      (probability == 0.5714285714285715) && (titleClass == 'Song) should be (true)
    }
  }

  feature("I can use my maximum likelyhood bayes learner as spam filter.") {
    scenario("I retrieve the underlying actor and call train and receive directly") {
      given("I can get the actor for the spamAndHamLearningRef")
      val spamAndHamLearner = spamAndHamLearnerRef.underlyingActor

      when("I call train the learner has stored the training data")
      spamAndHamLearner.train(spamAndHamDataSet)
      spamAndHamLearner.bayesNet.isEmpty must be (false)
      spamAndHamLearner.valueSet.size must be (12)

      then("I can classify messages based on the training set.")
      var result = spamAndHamLearner.predict("Sports")
      (result._2 == 0.8333333333333334) && (result._1 == 'Ham) must be (true)
      result = spamAndHamLearner.predict("Secret Is Secret")
      (result._2 == 0.9615384615384616) && (result._1 == 'Spam) must be (true)

      and("A query not in the spam bag of words must yield 'Ham with probability 1.0")
      result = spamAndHamLearner.predict("Today Is Secret")
      (result._1 == 'Ham) && (result._2 == 1.0) must be(true)
    }
  }

  feature("I can prevent prevent 1 word of determining the whole classification by using laplacian smoothing") {
    info("I call the bayes learner with laplacian smoothing using the actor interface")
    scenario("I have a trained network with laplacian smoothing factor of 1.0") {
      then("I can call predict with \"Today Is Secret\" through the actor interface")
      val result = (spamAndHamLearnerRefWithLaplaceRef ? Predict("Today Is Secret")).as[Prediction].getOrElse(throw new Exception("Unable to retrieve prediction"))

      and("The result is 'Ham with a certainty of 0.5142428785607196")
      val (classification, probability) = result.prediction
      (classification == 'Ham) && (probability == 0.5142428785607196) must be(true)
    }
  }
}

object MovieAndSongFactory {
  private def createMovieAndSongDataSet() = {
    val data = HashMap[Symbol, List[String]](
    ('Movie -> List("A Perfect World", "My Perfect Woman", "Pretty Woman")),
    ('Song -> List("A Perfect Day", "Electric Storm", "Another Rainy Day"))
    )
    DataSet(data)
  }
  def apply() = createMovieAndSongDataSet()
}

object SpamAndHamFactory {
  def createSpamAndHamDataSet() = {
    val data = HashMap[Symbol, List[String]](
    ('Spam -> List("Offer Is Secret", "Click Secret Link", "Secret Sports Link")),
    ('Ham -> List("Play Sports Today", "Went Play Sports", "Secret Sports Event", "Sports Is Today", "Sports Cost Money"))
    )
    DataSet(data)
  }

  def apply() = createSpamAndHamDataSet()
}