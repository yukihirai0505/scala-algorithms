package com.example.search

import scala.collection.mutable
import scala.math.floor

/**
  * Hashing method
  *
  * this is sample hash, it is not perfect hashing method. just practice.
  *
  * Created by yukihirai on 2017/04/02.
  */
class HashingMethod(list: List[Int]) {
  val seed = 1.5
  val resultMap: Map[Int, Int] = {
    var map: mutable.Map[Int, Int] = mutable.Map()

    def setValue(key: Int, v: Int): Unit = {
      if (map.get(key).isEmpty) map.put(key, v)
      else setValue(key + 1, v)
    }

    list.foreach { v =>
      val key = getKey(v, list.length)
      setValue(key, v)
    }
    map.map(kv => (kv._1, kv._2)).toMap
  }

  def getKey(v: Int, listLength: Int): Int = {
    floor(v % (listLength * seed)).toInt
  }

  private def getValueFromKey(key: Int) = {
    resultMap.get(key)
  }

  def getValue(v: Int): Option[Int] = {
    val key = getKey(v, list.length)
    (0 to 2).find(x => getValueFromKey(key + x).contains(v))
  }
}
