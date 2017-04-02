package com.example.search

/**
  * Linear Search https://en.wikipedia.org/wiki/Linear_search
  *
  * Created by yukihirai on 2017/04/02.
  */
object LinearSearch {

  def linearSearch[A](list: List[A], key: A): Option[A] = {

    def search(as: List[A]): Option[A] =
      if (as.isEmpty) None
      else if (as.head == key) Some(as.head)
      else search(as.tail)

    search(list)
  }
}
