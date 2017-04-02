package search

import com.example.search.BinarySearch
import org.scalatest._

class BinarySearchSpec extends FlatSpec with Matchers {
  "BinarySearch" should "binarySearch Found" in {
    val list = List(4, 2, 3, 5, 1).sorted
    BinarySearch.binarySearch[Int](list, 5) === Some(5)
  }
  "BinarySearch" should "binarySearch NotFound" in {
    val list = List(4, 2, 3, 5, 1).sorted
    BinarySearch.binarySearch[Int](list, 9) === None
  }
}
