package main

object Introduction {

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

}
