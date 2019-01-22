package search

import com.example.search.SequentialSearch
import org.scalatest._

class SequentialSearchSpec extends FlatSpec with Matchers {
  "LinearSearch" should "linearSearch Found" in {
    val list = List(4, 2, 3, 5, 1)
    SequentialSearch.linearSearch[Int](list, 5) === Some(5)
  }
  "LinearSearch" should "linearSearch NotFound" in {
    val list = List(4, 2, 3, 5, 1)
    SequentialSearch.linearSearch[Int](list, 9) === None
  }
}
