package com.example.sort

/**
  * Best: O(n)
  * Average, Worst: O(n**2)
  *
  * Shifts elements greater than value to the right
  * Inserts value into proper location
  *
  * The optimal performance occurs when the array is already sorted,
  * and arrays sorted in reverse order the worst performance for Insertion Sort.
  */
object InsertionSort {
  def sort(numbers: List[Int], sorted: List[Int] = List.empty): List[Int] = {
    def insert(num: Int): List[Int] = {
      for (i <- sorted.length - 1 to 0 by -1) {
        if (num > sorted(i)) {
          val j = i + 1
          return sorted.take(j) ++ List(num) ++ sorted.drop(j)
        }
      }
      num +: sorted
    }

    numbers match {
      case h :: last =>
        if (sorted.isEmpty) sort(last, sorted :+ h)
        else {
          sort(last, insert(h))
        }
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
