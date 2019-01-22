package com.example.search

/**
  * Sequential Search
  *
  * Best: O(1)
  * Average, Worst: O(n)
  *
  * 1. Access each element in order, from position 0 to n - 1
  * 2. Iterator continues until is exhausted of elements
  * 3. Each element is retrieved one by one from an iterator
  *
  */
object SequentialSearch {

  def linearSearch[A](list: List[A], key: A): Option[A] = {

    def search(as: List[A]): Option[A] =
      if (as.isEmpty) None
      else if (as.head == key) Some(as.head)
      else search(as.tail)

    search(list)
  }
}
