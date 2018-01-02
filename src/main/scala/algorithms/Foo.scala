package algorithms

object Foo {

  def main(args: Array[String]): Unit = {
    println(countChange(4, List(1, 2)))
    // 5

    println(countChange(300, List(5, 10, 20, 50, 100, 200, 500)))

    teste(x => x * x, 1, 2)
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money > 0 && coins.nonEmpty) countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else 0
  }

  def countWords() {
    val textFile = List("Lorem Ipsum is simply dummy text of the printing and typesetting industry." +
      " Lorem Ipsum has been the industry's standard dummy text ever since the 1500s")
    val counts = textFile.flatMap(line => line.split(" ")).groupBy(identity).mapValues(_.size)
    print(counts)
  }

  def teste(f: Int => Int, a: Int, b: Int) = 1

}
