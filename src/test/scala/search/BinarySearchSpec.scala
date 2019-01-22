package search

import com.example.search.BinarySearch
import org.scalatest._

class BinarySearchSpec extends FlatSpec with Matchers {
  "BinarySearch" should "binarySearch Found" in {
    val list = List(1, 2, 3, 4, 5)
    BinarySearch.binarySearch(list, 5) === Some(5)
  }
  "BinarySearch" should "binarySearch NotFound" in {
    val list = List(1, 2, 3, 4, 5)
    BinarySearch.binarySearch(list, 9) === None
  }
}
