package com.example.sort

/**
  * Best: O(n**2)
  * Average, Worst: O(n**2)
  *
  * It requires quadratic time even in the best case (i.e., when the array is already sorted).
  *
  */
object SelectionSort {
  def sort(numbers: List[Int], sorted: List[Int] = List.empty): List[Int] = {
    numbers match {
      case h :: last =>
        def findMin(minIndexAndNum: (Int, Int), index: Int = 0): (Int, Int) = {
          if (last.size == index) minIndexAndNum
          else findMin(
            if (minIndexAndNum._2 < last(index)) minIndexAndNum else (index, last(index)),
            index + 1
          )
        }

        val min = findMin((0, h))
        // find min and if it's not head swap it
        if (min._2 == h) sort(last, sorted :+ min._2)
        else sort(last.updated(min._1, h), sorted :+ min._2)
      case _ => sorted
    }
  }

  def main(args: Array[String]): Unit = {
    println(sort(List(8, 4, 3, 7, 6, 5, 2, 1)))
    println(sort(List(1, 9, 8, 4, 3, 10, 7, 6, 5, 2)))
    println(sort(List(1, 2, 3, 4)))
    println(sort(List(4, 3, 2, 1)))
    println(sort(List(2, 5, 5, 3, 3, 2, 2, 1)))
  }
}
