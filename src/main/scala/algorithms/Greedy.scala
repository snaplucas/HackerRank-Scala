package algorithms

object Greedy {

  def main(args: Array[String]): Unit = {
    println(maximumToys(Array(1, 12, 5, 111, 200, 1000, 10), 50))
  }

  def maximumToys(prices: Array[Int], k: Int): Int = {
    def toys(prices: Array[Int], acc: Int, used: Int): Int = {
      if (prices.isEmpty) prices.length
      else if (used + prices.head >= k) acc
      else toys(prices.tail, acc + 1, used + prices.head)
    }

    toys(prices.sorted, 0, 0)
  }

}
