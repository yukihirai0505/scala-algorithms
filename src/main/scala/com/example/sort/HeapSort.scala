package com.example.sort

/**
  * Best: O(n log n)
  * Average, Worst: O(n log n)
  *
  * 1. Assume parent A[idx] is larger than or equal to either of its children
  * 2. Left child is larger than its parent
  * 3. Right child is larger than either its parent or left sibling.
  */
object HeapSort {

  def swap(a: Array[Int], i: Int, j: Int): Unit = {
    val t = a(i)
    a(i) = a(j)
    a(j) = t
  }

  def heapify(a: Array[Int], idx: Int, max: Int): Unit = {
    var largest = idx
    val left = (2 * idx) + 1
    val right = 2 * idx + 2

    if (left < max && a(left) > a(idx)) {
      largest = left
    }
    if (right < max && a(right) > a(largest)) {
      largest = right
    }
    if (largest != idx) {
      swap(a, idx, largest)
      heapify(a, largest, max)
    }
  }

  // there are two ways to create heap structure => O(N) & O(N log N)
  def buildHeap(a: Array[Int], size: Int): Unit = {
    val k = size / 2
    for (idx <- k to 0 by -1) {
      heapify(a, idx, size)
    }
  }

  def heapSort(a: Array[Int]): Array[Int] = {
    buildHeap(a, a.length)
    for (i <- a.indices.reverse) {
      // swap max to idx
      swap(a, 0, i)
      // create heap structure again
      heapify(a, 0, i)
    }
    a
  }

  def main(args: Array[String]): Unit = {
    heapSort(Array(1, 0, 2, 9, 3, 8, 4, 7, 5, 6))
    heapSort(Array(3, 8, 4, 7, 5, 6, 10, 1, 0, 2, 9))
  }
}
