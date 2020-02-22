import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ReverserSpec extends AnyFlatSpec with Matchers {

  object TestObject

  val testFunc = (x: Int) => x + x

  "Reverser" should
    "reverse non empty list of ints" in {
    Reverser.reverse(List(1, 2, 3, 4, 5)) mustBe List(5, 4, 3, 2, 1)
  }

  /** this test doesn't add that much value but I added it just for fun (there's never too many tests :)!) */
  it should "reverse non empty list of any type" in {
    val anyList: List[Any] = List(1, "anyString", Map.empty, TestObject, testFunc)
    val expectedResult = List(testFunc, TestObject, Map.empty, "anyString", 1)
    Reverser.reverse(anyList) mustBe expectedResult
  }

  it should "return an empty list when such passed as parameter" in {
    Reverser.reverse(List.empty) mustBe List.empty
  }
}
