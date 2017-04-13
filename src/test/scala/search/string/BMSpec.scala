package search.string

import com.example.search.string.BM
import org.scalatest._

class BMSpec extends FlatSpec with Matchers {
  "BM" should "BM search Found" in {
    val txt = "I miss you so match."
    val pat = "you"
    BM.search(txt, pat) == true
  }
  "BM" should "BM search NotFound" in {
    val txt = "I miss you so match."
    val pat = "omg"
    BM.search(txt, pat) == false
  }
}
