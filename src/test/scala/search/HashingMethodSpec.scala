package search

import com.example.search.HashingMethod
import org.scalatest._

class HashingMethodSpec extends FlatSpec with Matchers {
  "HashingMethod" should "hasingMethod Found" in {
    val targetValue = 20
    val hashMethod = new HashingMethod(list = List(12, 25, 36, targetValue, 30, 8, 42))
    hashMethod.getValue(targetValue) === Some(targetValue)
  }
  "HashingMethod" should "hasingMethod NotFound" in {
    val hashMethod = new HashingMethod(list = List(12, 25, 36, 20, 30, 8, 42))
    val targetValue = 99
    hashMethod.getValue(targetValue) === None
  }
}
