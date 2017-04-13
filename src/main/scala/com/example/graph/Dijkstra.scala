package com.example.graph

/**
  * author Yuki Hirai on 2017/04/12.
  */
object Dijkstra extends App {

  import scala.annotation.tailrec

  val distance = List(
    List(0, 1, 2, 0, 0, 0),
    List(1, 0, 1, 2, 0, 0),
    List(2, 1, 0, 0, 1, 0),
    List(0, 2, 0, 0, 2, 2),
    List(0, 0, 1, 2, 0, 1),
    List(0, 0, 0, 2, 1, 0)
  )

  val init = List(0, -1, -1, -1, -1, -1)

  def getMinDistance(list: List[Int])(i: Int): Int =
    list.indices.toList
      .filter(list(_) != -1)
      .filter(distance(_)(i) != 0)
      .map(j => distance(i)(j) + list(j))
      .min

  def isIsolated(list: List[Int])(i: Int): Boolean =
    !list.indices.toList
      .filter(list(_) != -1).exists(distance(_)(i) != 0)

  def getNextMinPosition(list: List[Int]): Int =
    list.indices.toList
      .filter(list(_) == -1)
      .filterNot(isIsolated(list))
      .minBy(getMinDistance(list))

  def compute(l: List[Int]): List[Int] = {
    @tailrec
    def rec(list: List[Int], restCount: Int): List[Int] = {
      restCount match {
        case 0 => list
        case _ =>
          val next = getNextMinPosition(list)
          val dis = getMinDistance(list)(next)
          rec(list.updated(next, dis), restCount - 1)
      }
    }

    rec(l, l.size - 1)
  }

  println(compute(init))
}
