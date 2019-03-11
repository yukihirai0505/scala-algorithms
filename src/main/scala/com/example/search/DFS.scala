package com.example.search


class Graph[T](g: Map[T, List[T]]) {
  type Vertex = T

  def DFS(start: Vertex): List[Vertex] = {

    def DFS0(v: Vertex, visited: List[Vertex]): List[Vertex] = {
      if (visited.contains(v))
        visited
      else {
        val neighbours: List[Vertex] = g(v) filterNot visited.contains
        neighbours.foldLeft(v :: visited)((b, a) => DFS0(a, b))
      }
    }

    DFS0(start, List()).reverse
  }
}

object DFS {

  def main(args: Array[String]): Unit = {
    /*

         |3|
    |1|       |2|
  |5| |4|    |6|

     */
    val intGraph = new Graph[Int](
      Map(
        3 -> List(1, 2),
        1 -> List(5, 4),
        4 -> List(),
        5 -> List(),
        2 -> List(6),
        6 -> List()
      )
    )
    println(intGraph.DFS(3))
  }
}
