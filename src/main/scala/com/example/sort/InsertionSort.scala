package com.example.sort

/**
  * Best: O(n)
  * Average, Worst: O(n**2)
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
  }
}
