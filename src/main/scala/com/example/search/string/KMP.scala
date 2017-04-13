package com.example.search.string

import scala.annotation.tailrec

/**
  * author Yuki Hirai on 2017/04/13.
  */
object KMP {

  def search(text: Array[Char], pattern: Array[Char], R: Int = 256): Int = {
    val M = pattern.length
    val dfa = Array.ofDim[Int](R, M)
    dfa(pattern(0))(0) = 1
    var X = 0
    for (j <- 1 until M; c <- 0 until R) {
      dfa(c)(j) = dfa(c)(X)
      dfa(pattern(j))(j) = j + 1
      X = dfa(pattern(j))(X)
    }
    val N = text.length

    @tailrec
    def loop(i: Int, j: Int): (Int, Int) =
      if (i < N && j < M) loop(i + 1, dfa(text(i))(j)) else (i, j)

    val tuple = loop(0, 0)
    if (tuple._2 == M) tuple._1 - M else N
  }

}
