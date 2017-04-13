package com.example.search.string

object BM {

  def search(text: String, pattern: String): Boolean = {
    val textLength = text.length
    val patternLength = pattern.length
    val table = makeTable(pattern)

    def inner(patternIndex: Int, textIndex: Int, tailIndex: Int): Boolean = {
      if (textIndex >= textLength) false
      else {
        if (patternIndex < 0) {
          true
        } else {
          val (nextPatternIndex, nextTextIndex, nextTailIndex) = nextIndex(patternIndex, textIndex, tailIndex)
          inner(nextPatternIndex, nextTextIndex, nextTailIndex)
        }
      }
    }

    def nextIndex(patternIndex: Int, textIndex: Int, tailIndex: Int): (Int, Int, Int) = {
      val charOfText = text(textIndex)
      if (charOfText == pattern(patternIndex))
        (patternIndex - 1, textIndex - 1, tailIndex)
      else {
        val nextPatternIndex = patternLength - 1
        table.get(charOfText) match {
          case None => (nextPatternIndex, tailIndex + patternLength, tailIndex + patternLength)
          case Some(shiftFromTail) =>
            if ((patternLength - shiftFromTail - 1) >= patternIndex)
              (nextPatternIndex, tailIndex + 1, tailIndex + 1)
            else
              (nextPatternIndex, tailIndex + shiftFromTail, tailIndex + shiftFromTail)
        }
      }
    }

    inner(patternLength - 1, patternLength - 1, patternLength - 1)
  }

  private def makeTable(pattern: String): Map[Char, Int] = {
    pattern.reverse.toList.zipWithIndex.reverse.toMap
  }
}
