package com.example.dijkstra

import scala.annotation.tailrec

/** *
  *
  * The Graph is following
  *
  * START
  * ├2─A
  * │  ├2─E
  * │  │ └2─GOAL(6)
  * │  ├1─D
  * │  │ └1─GOAL(4)
  * │  └3─GOAL(5)
  * └1─B
  * └6─GOAL(7)
  */
case class Graph(nodes: Map[String, Seq[Node]])

case class Node(name: String, weight: Int, from: Option[Node] = None) extends Ordered[Node] {
  def compare(e: Node): Int = e.weight - weight
}

object Dijkstra extends App {
  val sampleGraph: Graph = {
    val (start, goal, a, b, c, d) = ("START", "GOAL", "A", "B", "C", "D")
    Graph(
      Map(
        start -> Seq(Node(a, 2), Node(b, 1)),
        a -> Seq(Node(c, 2), Node(d, 1), Node(goal, 3)),
        c -> Seq(Node(goal, 2)),
        d -> Seq(Node(goal, 1)),
        b -> Seq(Node(goal, 6))
      )
    )
  }
  dikstra(sampleGraph).foreach(println)
  type Path = Map[String, Node]

  def dikstra(graph: Graph): Seq[String] = {
    val start = "START"
    val goal = "GOAL"
    val path = dikstra(graph, start)
    // 最短距離を文字列で取得
    path.get(goal).map(shortestPath(_)).getOrElse(Nil)
  }

  @tailrec
  def shortestPath(e: Node, r: Seq[String] = Nil): Seq[String] = e.from match {
    case Some(ef) => shortestPath(ef, e.name +: r)
    case None => e.name +: r
  }

  def dikstra(graph: Graph, start: String): Path = {
    val startNode = Node(start, 0)
    val q = scala.collection.mutable.PriorityQueue[Node]()
    q += startNode

    @tailrec
    def search(path: Path = Map.empty): Path = {
      println(q)
      println(path)
      // 遷移先がなくなるまで
      if (q.isEmpty) path
      else {
        val node = q.dequeue()
        graph.nodes.get(node.name).foreach { nodes =>
          nodes.foreach { n =>
            // 遷移先を登録
            if (!path.contains(n.name)) q += Node(n.name, n.weight + n.weight, Some(n))
          }
        }
        // 訪問済みなら小さいほう優先
        val ne = path.get(node.name).map(e => if (node.weight < e.weight) node else e).getOrElse(node)
        search(path + (node.name -> ne))
      }
    }

    search(Map(start -> startNode))
  }
}