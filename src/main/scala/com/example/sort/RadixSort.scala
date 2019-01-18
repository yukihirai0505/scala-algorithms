package com.example.sort

/**
  *
  * Best: Ω(nk)
  * Average, Worst: O(nk)
  *
  */
object RadixSort {

  def getKey(value: Int, digit: Int): Int = {
    (value % scala.math.pow(10, digit)).toInt / scala.math.pow(10, digit - 1).toInt
  }

  def sort(a: List[Int], maxDigits: Int): List[Int] = {
    def radixSort(sorted: List[Int], digit: Int = 1): List[Int] = {
      if (digit > maxDigits) sorted
      else {
        val radix: Array[Array[Int]] = Array.fill(10) {
          Array.empty
        }
        sorted.foreach(num => {
          val key = getKey(num, digit)
          val radixSet = radix(key)
          radix(key) = radixSet :+ num
        })
        radixSort(radix.flatten.toList, digit + 1)
      }
    }

    radixSort(a)
  }

  def main(args: Array[String]): Unit = {
    println(
      sort(
        List(3221, 1, 10, 9680, 577, 9420, 7, 5622, 4793, 2030, 3138, 82, 2599, 743, 4127), 4
      )
    )
  }
}
