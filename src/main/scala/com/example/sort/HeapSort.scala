package com.example.sort

/**
  * Best, Average, Worst: O(n log n)
  *
  * 1. Assume parent A[idx] is larger than or equal to either of its children
  * 2. Left child is lesser than its parent
  * 3. Right child is larger than left sibling.
  *
  * Good movie => https://www.youtube.com/watch?v=X0ESspSiLIc
  */
object HeapSort {

  def swap(a: List[Int], i: Int, j: Int): List[Int] = {
    a.updated(i, a(j)).updated(j, a(i))
  }

  def heapify(a: List[Int], idx: Int, max: Int): List[Int] = {
    var largest = idx
    val left = (2 * idx) + 1
    val right = 2 * idx + 2

    if (left < max && a(left) > a(idx)) {
      largest = left
    }
    if (right < max && a(right) > a(largest)) {
      largest = right
    }
    // until idx == largest swap and heapify
    if (largest == idx) a
    else heapify(swap(a, idx, largest), largest, max)
  }

  def buildHeap(a: List[Int]): List[Int] = {
    val k = a.length / 2 - 1

    def loop(heap: List[Int], idx: Int): List[Int] = {
      if (idx < 0) heap
      else loop(heapify(heap, idx, a.length), idx - 1)
    }

    loop(a, k)
  }

  def heapSort(a: List[Int]): List[Int] = {
    val heap = buildHeap(a)

    def loop(sorted: List[Int], idx: Int): List[Int] = {
      if (idx < 0) sorted
      else loop(heapify(swap(sorted, 0, idx), 0, idx), idx - 1)
    }

    loop(heap, heap.size - 1)
  }

  def main(args: Array[String]): Unit = {
    println(heapSort(List(1, 0, 2, 9, 3, 8, 4, 7, 5, 6)))
    // -> heap: 1,0,2,9,3,8,4,7,5,6 6 and 3
    // -> heap: 1,0,2,9,6,8,4,7,5,3 8 and 2
    // -> heap: 1,0,8,9,6,2,4,7,5,3 9 and 0
    // -> heap: 1,9,8,0,6,2,4,7,5,3 7 and 0
    // -> heap: 1,9,8,7,6,2,4,0,5,3 9 and 1
    // -> heap: 9,1,8,7,6,2,4,0,5,3 7 and 1
    // -> heap: 9,7,8,1,6,2,4,0,5,3 5 and 1
    // -> heap: 9,7,8,5,6,2,4,0,1,3
    println(heapSort(List(3, 8, 4, 7, 5, 6, 10, 1, 0, 2, 9)))
    // -> heap: 10,9,6,7,8,3,4,1,0,2,5
  }
}
