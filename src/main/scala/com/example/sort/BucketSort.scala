package com.example.sort

/**
  * Best, Average, Worst: O(n)
  *
  * 1. Create bucket list and hash all elements to proper bucket
  * 2. Process all buckets to extract values back into A in sorted order
  * 3. If more than one element int bucket, sort first
  * 4. Copy elements back into proper position in A
  *
  */
object BucketSort {

  def bucketSort(a: List[Int]): List[Int] = {
    val bucket: Array[Array[Int]] = Array.fill(a.size) {
      Array.empty
    }
    a.foreach(num => {
      val n = bucket(num)
      bucket(num) = n :+ num
    })

    def sort(bucket: Array[Array[Int]], sorted: List[Int] = List.empty): List[Int] = {
      if (bucket.isEmpty) sorted
      else {
        sort(bucket.drop(1), sorted ++ bucket.head)
      }
    }

    sort(bucket)
  }

  def main(args: Array[String]): Unit = {
    println(
      bucketSort(List(3, 6, 9, 0, 5, 1, 8, 4, 3, 1))
    )
  }
}
