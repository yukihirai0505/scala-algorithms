package search.string

import com.example.search.string.BruteForceStringMatch
import org.scalatest._

class BruteForceStringMatchSpec extends FlatSpec with Matchers {
  "BruteForceStringMatch" should "bruteForceStringMatch Found" in {
    val txt = "I miss you so match."
    val pat = "you"
    BruteForceStringMatch.bruteForceStringMatch(txt, pat) == true
  }
  "BruteForceStringMatch" should "bruteForceStringMatch NotFound" in {
    val txt = "I miss you so match."
    val pat = "omg"
    BruteForceStringMatch.bruteForceStringMatch(txt, pat) == false
  }
}
