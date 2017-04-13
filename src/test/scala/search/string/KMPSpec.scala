package search.string

import com.example.search.string.KMP
import org.scalatest._

class KMPSpec extends FlatSpec with Matchers {
  "KMP" should "KMP search Found" in {
    val txt = "I miss you so match.".toCharArray
    val pat = "you".toCharArray
    KMP.search(txt, pat) == pat.length
  }
  "KMP" should "KMP search NotFound" in {
    val txt = "I miss you so match.".toCharArray
    val pat = "omg".toCharArray
    KMP.search(txt, pat) == 0
  }
}
