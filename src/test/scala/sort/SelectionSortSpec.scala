package sort

import com.example.sort.SelectionSort
import org.scalatest._

class SelectionSortSpec extends FlatSpec with Matchers {
  "SelectionSort" should "Sorted" in {
    val list = List(4, 2, 3, 5, 1)
    SelectionSort.sort(list) === List(1, 2, 3, 4, 5)
  }
}
