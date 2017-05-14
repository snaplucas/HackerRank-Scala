package main

object Introduction {

  def main(args: Array[String]): Unit = {
    println(reverseList(List(1, 2, 3)))
  }

  def listReplication1(num: Int, arr: List[Int]): List[Int] = {
    arr.flatMap(x => repeatN(num, x, Nil))
  }

  def repeatN(num: Int, item: Int, acc: List[Int]): List[Int] = {
    num match {
      case 0 => acc
      case _ => repeatN(num - 1, item, item :: acc)
    }
  }

  def listReplication2(num: Int, arr: List[Int]) = for {e <- arr; _ <- 1 to num} yield e

  def sumOdds(arr: List[Int]): Int = {
    def loop(sum: Int, arr: List[Int]): Int =
      if (arr.isEmpty) sum
      else if (arr.head % 2 != 0) loop(sum + arr.head, arr.tail)
      else loop(sum, arr.tail)

    loop(0, arr)
  }

  def sumOdds2(arr: List[Int]): Int = arr.filter(a => a % 2 == 1).sum

  def filterArray1(delim: Int, arr: List[Int]) = arr.filter(a => a < delim)

  def filterArray2(delim: Int, arr: List[Int]) = for (e <- arr if e < delim) yield e

  def filterArray3(delim: Int, arr: List[Int]): List[Int] =
    if (arr.isEmpty) List()
    else if (arr.head < delim) List(arr.head) ::: filterArray3(delim, arr.tail)
    else filterArray3(delim, arr.tail)

  def filterPositionList(arr: List[Int]): List[Int] = {
    def loop(iter: Int, arr: List[Int]): List[Int] =
      if (arr.isEmpty) arr
      else if (iter % 2 == 1) arr.head :: loop(iter + 1, arr.tail)
      else loop(iter + 1, arr.tail)

    loop(0, arr)
  }

  def updateList(arr: List[Int]): List[Int] =
    if (arr.isEmpty) arr else if (arr.head < 0) -arr.head :: updateList(arr.tail)
    else arr.head :: updateList(arr.tail)

  def arrayOfN(num: Int): List[Int] = (for (i <- 1 to num) yield i).toList

  def arrayOfN2(num: Int): List[Int] = (1 to num).toList

  def reverseList(arr: List[Int]): List[Int] = arr match {
    case head :: tail => reverseList(tail) ::: List(head)
    case Nil => Nil
  }

}
