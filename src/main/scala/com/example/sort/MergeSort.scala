package com.example.sort

/**
  * Best, Average, Worst: O(n log n)
  *
  * 1. Make full copy of all elements
  * 2. Place elements of A[start, end] into result[start, end] in sorted order
  * 3. Sort results[start, mid] into A[start, mid]
  * 4. Merge sorted subarrays in A back into result
  *
  * Good document => http://www.ics.kagoshima-u.ac.jp/~fuchida/edu/algorithm/sort-algorithm/merge-sort.html
  */
object MergeSort {

  def merge(left: List[Int], right: List[Int]): List[Int] =
    (left, right) match {
      case (_, Nil) => left
      case (Nil, _) => right
      case (leftHead :: leftTail, rightHead :: rightTail) =>
        if (leftHead < rightHead) leftHead :: merge(leftTail, right)
        else rightHead :: merge(left, rightTail)
    }

  def mergeSort(list: List[Int]): List[Int] = {
    val mid = list.length / 2
    if (mid == 0) list
    else {
      val (left, right) = list.splitAt(mid)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def main(args: Array[String]): Unit = {
    println(
      mergeSort(List(3, 6, 9, 0, 5, 1, 8, 4, 3, 1))
    )
  }
}
