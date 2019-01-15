package CrackingTheCodingInterview

object One {
  def solution(str: String): Boolean = {
    str.length == str.toSet.size
  }

  def main(args: Array[String]): Unit = {
    println(solution("abcdefg"))
    println(solution("aabbccdd"))
  }
}
