package com.example

object Hello extends App {

  case class Town(name: String, to: List[(String, Int)] = Nil) {
    var dist = 30
    var set = false
    var from = ""
    override def toString = name + "(" + dist + "):" + route(name)
  }

  val START = "START"

  val tS = Town(START, List(("A", 5), ("B", 4), ("C", 3)))
  val tA = Town("A", List(("B", 2), ("GOAL", 6)))
  val tB = Town("B", List(("A", 2), ("C", 3), ("D", 2)))
  val tC = Town("C", List(("B", 3), ("D", 6)))
  val tD = Town("D", List(("B", 2), ("GOAL", 4), ("C", 6)))
  val tG = Town("GOAL")
  val towns = List(tS, tA, tB, tC, tG)

  tS.dist = 0
  makeMap(tS)
  towns foreach println

  def makeMap(t: Town) {
    t.set = true
    t.to.foreach(r => getTown(r._1) match {
      case Some(x) if x.dist > t.dist + r._2 =>
        x.dist = t.dist + r._2
        x.from = t.name
      case _       =>
    })
    if (towns exists (!_.set)) makeMap(towns filter (!_.set) minBy (_.dist))
  }

  def route(tn: String): String = getTown(tn) match {
    case Some(Town(START, _)) => START
    case Some(x)              => route(x.from) + " -> " + tn
    case _                    => "Integerの果て"
  }

  def getTown(targ: String): Option[Town] = towns find (targ == _.name)

}
