object StringCompactor {

  /**
    * This methods do not guard against incorrect format. Task description seems to suggest that parameters will already be in
    * proper format. If that's not the case such change would not be a problem to implement and test and I am happy to do so
    * if required.
    */

  def compact(param: String): String = {
    def compactAux(str: String, currChar: Char, counter: Int = 0, result: String = ""): String = (str, currChar) match {
      case ("", char) => if (counter > 1) s"$result$counter$char" else s"$result$char"
      case (s, char) if s.head == char => compactAux(s.tail, currChar, counter + 1, result)
      case (s, char) => compactAux(s, s.head, 0, if (counter > 1) s"$result$counter$char" else s"$result$char")
    }

    if (param.isEmpty) "" else compactAux(param, param.head)
  }

  def decompact(param: String): String = {
    def decompactAux(str: String, result: String = ""): String = str match {
      case "" => result
      case s if s.head.isDigit => decompactAux(str.substring(2), result + (str(1).toString * s.head.asDigit))
      case s => decompactAux(str.tail, s"$result${s.head}")
    }

    if (param.isEmpty) "" else decompactAux(param)
  }
}