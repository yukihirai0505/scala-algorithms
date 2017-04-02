package com.example.sort

/**
  * Selection Sort http://en.wikipedia.org/wiki/Selection_sort
  *
  * Created by yukihirai on 2017/04/02.
  */
object SelectionSort {

  def selectionSort[A <% Ordered[A]](list: List[A]): List[A] = {
    def sort(orgList: List[A], sortedList: List[A]): List[A] = orgList match {
      case h :: t => select(head = h, tail = t, Nil, sortedList)
      case Nil => sortedList
    }
    def select(head: A, tail: List[A], orgList: List[A], sortedList: List[A]): List[A] = {
      tail match {
        case h :: t =>
          if (head > h) select(head = head, tail = t, h :: orgList, sortedList)
          else select(h, t, head :: orgList, sortedList)
        case Nil => sort(orgList = orgList, sortedList = head :: sortedList)
      }
    }
    sort(list, Nil)
  }
}
