package com.example.sort

/**
  * Best, Average: O(n log n)
  * Worst: O(n**2)
  *
  */
object QuickSort {

  def quickSort(a: List[Int]): List[Int] = {
    if (a.length > 1) {
      val pivot = a(a.length / 2)
      println(pivot)
      List.concat(
        quickSort(a filter (pivot > _)),
        a filter (pivot == _),
        quickSort(a filter (pivot < _))
      )
    } else a
  }

  def main(args: Array[String]): Unit = {
    val a = List(1, 0, 2, 9, 3, 8, 4, 7, 5, 6)
    // 1,0,2,9,3,8,4,7,5,6
    // pivot => 8
    // |1,0,2,3,4,7,5,6|8|9|
    // pivot => 4
    // |1,0,2,3|4|7,5,6|8|9|
    // pivot => 2, 5
    // |1,0|2|3|4|5|7,6|8|9|
    // pivot => 0, 6
    // |0|1|2|3|4|5|6|7|8|9|
    println(quickSort(a))
    //    val b = List(1, 0, 2, 9, 3, 8, 4, 7, 5, 6, 10)
    //    println(quickSort(b))
  }
}
