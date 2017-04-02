package search

import com.example.search.LinearSearch
import org.scalatest._

class LinearSearchSpec extends FlatSpec with Matchers {
  "LinearSearch" should "linearSearch Found" in {
    val list = List(4, 2, 3, 5, 1)
    LinearSearch.linearSearch[Int](list, 5) === Some(5)
  }
  "LinearSearch" should "linearSearch NotFound" in {
    val list = List(4, 2, 3, 5, 1)
    LinearSearch.linearSearch[Int](list, 9) === None
  }
}
