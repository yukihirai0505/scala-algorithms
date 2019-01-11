package com.example.sort
/**
  * Best: O(n)
  * Average, Worst: O(n**2)
  */
object InsertionSort {
  def sort(numbers: List[Int], sorted: List[Int] = List.empty): List[Int] = {
    def insert(num: Int): List[Int] = {
      for ((tmp, i) <- sorted.zipWithIndex) {
        if (num < tmp) {
          return sorted.take(i) ++ List(num) ++ sorted.drop(i)
        }
      }
      sorted :+ num
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
