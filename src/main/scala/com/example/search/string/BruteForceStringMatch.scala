package com.example.search.string

/**
  * author Yuki Hirai on 2017/04/13.
  */
object BruteForceStringMatch {
  def bruteForceStringMatch(txt: String, pat: String): Boolean = {
    txt.indices.exists(i => txt.substring(i).startsWith(pat))
  }
}
