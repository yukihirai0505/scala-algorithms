package com.example.sort

/**
  * Selection Sort http://en.wikipedia.org/wiki/Selection_sort
  *
  * Created by yukihirai on 2017/04/02.
  */
object SelectionSort {

  def selectionSort[A <% Ordered[A]](list: List[A]): List[A] = {
    def sort(as: List[A], bs: List[A]): List[A] = as match {
      case h :: t => select(h, t, Nil, bs)
      case Nil => bs
    }
    def select(m: A, as: List[A], zs: List[A], bs: List[A]): List[A] = {
      as match {
        case h :: t =>
          if (m > h) select(m, t, h :: zs, bs)
          else select(h, t, m :: zs, bs)
        case Nil => sort(zs, m :: bs)
      }
    }
    sort(list, Nil)
  }
}
