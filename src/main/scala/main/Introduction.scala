package main

object Introduction {

  def main(args: Array[String]): Unit = {
    println(sumOdds2(List(3, 2, 4, 6, 5, 7, 8, 0, 1)))
  }

  def repeatN(num: Int, item: Int, acc: List[Int]): List[Int] = {
    num match {
      case 0 => acc
      case _ => repeatN(num - 1, item, item :: acc)
    }
  }

  def listReplication1(num: Int, arr: List[Int]): List[Int] = {
    arr.flatMap(x => repeatN(num, x, Nil))
  }

  def listReplication2(num: Int, arr: List[Int]): List[Int] = for {e <- arr; _ <- 1 to num} yield e

  def sumOdds(arr: List[Int]): Int = {
    def loop(sum: Int, arr: List[Int]): Int =
      if (arr.isEmpty) sum
      else if (arr.head % 2 != 0) loop(sum + arr.head, arr.tail)
      else loop(sum, arr.tail)

    loop(0, arr)
  }

  def sumOdds2(arr: List[Int]): Int = arr.filter(a => a % 2 == 1).sum

}
