package com.example.sort

/**
  *
  * Best: O(n)
  * Average, Worst: O(n**2)
  *
  */
object BubbleSort {

  def swap(a: List[Int], index: Int = 1): List[Int] = {
    if (index == a.size) a
    else {
      val current = a(index)
      val prev = a(index - 1)
      if (current < prev) {
        swap(a.updated(index, prev).updated(index - 1, current), index + 1)
      } else swap(a, index + 1)
    }
  }

  def bubbleSort(a: List[Int], sorted: List[Int] = List.empty): List[Int] = {
    if (a.isEmpty) sorted
    else {
      val swapped = swap(a)
      bubbleSort(swapped.dropRight(1), swapped.last +: sorted)
    }
  }

  def main(args: Array[String]): Unit = {
    println(
      bubbleSort(List(3, 6, 9, 0, 5, 1, 8, 4, 3, 1))
    )
    println(
      bubbleSort(List(7, 6, 9, 0, 5, 10, 8, 4, 3, 1))
    )
  }
}
