package com.example.search

/**
  *
  * Binary Search
  *
  * Best: O(1)
  * Average, Worst: O(log n)
  *
  * 1. Repeat while there is a range to be seached
  * 2. Midpoint computed using integer arithmetic
  * 3. "Variations" discusses how to support a "search-or-insert" operation based on final value of mid at this point
  *
  */
object BinarySearch {

  def binarySearch(list: List[Int], target: Int): Option[Int] = {

    def search(low: Int, high: Int): Option[Int] = (low + high) / 2 match {
      case _ if high < low => None
      case mid if list(mid) > target => search(low, mid - 1)
      case mid if list(mid) < target => search(mid + 1, high)
      case mid => Some(mid)
    }

    search(0, list.size - 1)
  }
}
