package com.example.search

/**
  * Binary Search https://en.wikipedia.org/wiki/Binary_search_algorithm
  *
  * Created by yukihirai on 2017/04/02.
  */
object BinarySearch {

  def binarySearch[A <% Ordered[A]](list: List[A], key: A): Option[A] = {

    def search(beforeList: List[A], afterList: List[A]): Option[A] =
      if (beforeList == afterList) None
      else test(beforeList, afterList, middleList = middle(beforeList, afterList))

    def test(beforeList: List[A], afterList: List[A], middleList: List[A]): Option[A] =
      if (key < middleList.head) search(beforeList = beforeList, middleList)
      else if (key > middleList.head) search(beforeList = middleList.tail, afterList)
      else Some(middleList.head)

    def middle(beforeList: List[A], afterList: List[A]): List[A] = {
      def race(before: List[A], tail: List[A]): List[A] =
        if (tail != afterList && tail.tail != afterList)
          race(before = before.tail, tail = tail.tail.tail)
        else before
      race(before = beforeList, tail = beforeList.tail)
    }
    search(beforeList = list, afterList = Nil)
  }
}
