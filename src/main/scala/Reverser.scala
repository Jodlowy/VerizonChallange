object Reverser {

  def reverse[T](list: List[T]): List[T] = {
    def reverseAux(list: List[T], reversed: List[T] = List.empty): List[T] = list match {
      case Nil => reversed
      case _ => reverseAux(list.tail, list.head +: reversed)
    }
    reverseAux(list)
  }
}
