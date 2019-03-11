package com.example.search

class BFSGraph[T](g: Map[T, List[T]]) {
  type Vertex = T

  def BFS(start: Vertex): List[List[Vertex]] = {

    def BFS0(elems: List[Vertex], visited: List[List[Vertex]]): List[List[Vertex]] = {
      val newNeighbors = elems.flatMap(g(_)).filterNot(visited.flatten.contains).distinct
      if (newNeighbors.isEmpty)
        visited
      else
        BFS0(newNeighbors, newNeighbors :: visited)
    }

    BFS0(List(start), List(List(start))).reverse
  }
}

object BFS {

  def main(args: Array[String]): Unit = {
    /*

         |3|
    |1|       |2|
  |5| |4|    |6|

     */
    val intGraph = new BFSGraph[Int](
      Map(
        3 -> List(1, 2),
        1 -> List(5, 4),
        4 -> List(),
        5 -> List(),
        2 -> List(6),
        6 -> List()
      )
    )
    println(intGraph.BFS(3))
  }
}
