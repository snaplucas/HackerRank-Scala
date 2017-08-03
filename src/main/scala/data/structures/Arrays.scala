package data.structures

object Arrays {

  def main(args: Array[String]) {
    println(if (sequenceFullColors("RYBG")) "True" else "False")
  }

  def arraysDS(arr: Array[Int]) = arr.reverse.foreach(a => print(a + " "))

  def sequenceFullColors(balls: String) = {
    def sequence(ballsList: List[Char], redGreen: Int, yellowBlue: Int): Boolean = {
      if (ballsList.isEmpty) redGreen == 0 && yellowBlue == 0
      else if (ballsList.head == 'R') redGreen <= 1 && sequence(ballsList.tail, redGreen + 1, yellowBlue)
      else if (ballsList.head == 'G') redGreen <= 1 && sequence(ballsList.tail, redGreen - 1, yellowBlue)
      else if (ballsList.head == 'Y') yellowBlue <= 1 && sequence(ballsList.tail, redGreen, yellowBlue + 1)
      else if (ballsList.head == 'B') yellowBlue <= 1 && sequence(ballsList.tail, redGreen, yellowBlue - 1)
      else false
    }

    sequence(balls.toList, 0, 0)
  }

  def sparseArrays() = {}
}
