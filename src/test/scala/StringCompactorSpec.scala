import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class StringCompactorSpec extends AnyFlatSpec with Matchers {

  "compact method" should
    "return empty string when such passed as parameter" in {
    StringCompactor.compact("") mustBe ""
  }

  it should "return single letter when single letter passed" in {
    StringCompactor.compact("A") mustBe "A"
  }

  it should "return number of occurrences and the letter for string of multiple same letters passed" in {
    StringCompactor.compact("AA") mustBe "2A"
  }

  it should "return same string for singles" in {
    StringCompactor.compact("ABC") mustBe "ABC"
  }

  it should "return compacted string with multiple different letters" in {
    val testString = "AAAAABBBCCC"
    val expected = "5A3B3C"
    StringCompactor.compact(testString) mustBe expected
  }

  it should "return compacted string with no 'ones' " in {
    val testString = "AAAAABCDDD"
    val expected = "5ABC3D"
    StringCompactor.compact(testString) mustBe expected
  }

  "decompact method" should
    "return empty string when such passed as parameter" in {
    StringCompactor.decompact("") mustBe ""
  }

  it should "return single letter when single letter passed" in {
    StringCompactor.decompact("A") mustBe "A"
  }

  it should "return n number of letters for string with one number and letter" in {
    StringCompactor.decompact("2A") mustBe "AA"
  }

  it should "return same string for singles" in {
    StringCompactor.compact("ABC") mustBe "ABC"
  }

  it should "return decompacted string with multiple different letters" in {
    val testString = "5A3B3C"
    val expected = "AAAAABBBCCC"
    StringCompactor.decompact(testString) mustBe expected
  }

  it should "return decompacted string with no 'ones'" in {
    val testString = "5ABC3D"
    val expected = "AAAAABCDDD"
    StringCompactor.decompact(testString) mustBe expected
  }
}
