package algorithms

object WarmUp {

  def staircase(n: Int): Unit = {
    def stair(n: Int, acc: Int): Unit = {
      if (acc == 0) println('#'.toString * n)
      else {
        println((' '.toString * acc) + '#'.toString * (n - acc))
        stair(n, acc - 1)
      }
    }

    stair(n, n - 1)
  }

}
