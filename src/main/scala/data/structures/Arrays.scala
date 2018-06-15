package data.structures

object Arrays {

  def main(args: Array[String]): Unit = {
    println(leftRotation(List(41, 73, 89, 7, 10, 1, 59, 58, 84, 77, 77, 97, 58, 1, 86, 58, 26, 10, 86, 51), 10))

  }


  def arraysDS(arr: Array[Int]): Unit = arr.reverse.foreach(a => print(a + " "))

  def leftRotation(s: List[Int], n: Int) = {
    if (n == 0) s
    else {
      val (b, c) = s.splitAt(n)
      (c ++ b) mkString " "
    }
  }
}
