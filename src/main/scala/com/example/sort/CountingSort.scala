package com.example.sort

/**
  *
  * Best, Average, Worst: O(n + k)
  *
  */
object CountingSort {

  def countingSort(a: List[Int], max: Int): List[Int] = {
    val count: Array[Int] = Array.fill(max) {
      0
    }
    a.foreach(num => {
      val n = count(num)
      count(num) = n + 1
    })

    def sort(index: Int = 0, sorted: List[Int] = List.empty): List[Int] = {
      if (index == max) sorted
      else {
        sort(index + 1, sorted ++ (0 until count(index)).map(_ => index))
      }
    }

    sort()
  }

  def main(args: Array[String]): Unit = {
    println(
      countingSort(List(3, 6, 9, 0, 5, 1, 8, 4, 3, 1), 10)
    )
  }
}
