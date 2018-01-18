package functional.programming

object AdHoc {

  def rotateString(s: String): Unit = {
    def rotate(chars: List[Char], n: Int): Unit = {
      if (n != 0) {
        print(chars.tail.mkString + chars.head + " ")
        rotate(chars.tail ::: List(chars.head), n - 1)
      } else {
        println()
      }
    }

    rotate(s.toList, s.length)
  }

}
