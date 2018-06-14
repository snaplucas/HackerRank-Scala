package data.structures

object Arrays {

  def main(args: Array[String]): Unit = {
    val stdin = scala.io.StdIn
    val a = stdin.readLine.split(" ").map(_.trim.toInt)
    //     val rotations = if (n > s.size) n % s.size else n
    println(leftRotation(List(1, 2, 3, 4, 5), 4).toString())

  }


  def arraysDS(arr: Array[Int]): Unit = arr.reverse.foreach(a => print(a + " "))

  def leftRotation(s: List[Int], n: Int): List[Int] = {
    if (n == 0) s
    else leftRotation(s.tail ::: List(s.head), n - 1)
  }
}
