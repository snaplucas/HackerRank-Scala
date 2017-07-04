package algorithms

object Strings {

  def main(args: Array[String]) {

    println(makingAnagrams("cde", "abc"))

    println(gameOfThronesI("aabbcc"))

    println(if (hackerRankInAString("hacakaeararanaka".toList)) "YES" else "NO")
    println(if (hackerRankInAString("hhhhaaaaackkkkerrrrrrrrank".toList)) "YES" else "NO")
    println(if (hackerRankInAString("crackerhackerknar".toList)) "YES" else "NO")

    println(gemStones(List("abcd".toList, "abef".toList, "abjj".toList)))
  }

  val indices = Vector('h', 'a', 'c', 'k', 'e', 'r', 'r', 'a', 'n', 'k')

  def hackerRankInAString(s: List[Char]) = {
    def hackerrank(s: List[Char], acc: Int): Boolean = {
      if (s.isEmpty) acc == 10
      else if (s.head == indices(acc)) acc == 9 || hackerrank(s.tail, acc + 1)
      else hackerrank(s.tail, acc)
    }

    hackerrank(s, 0)
  }

  val alphas = Vector('a', 'b', 'c', 'd')

  def gemStones(stones: List[List[Char]]) = {
    def gem(stones: List[List[Char]], indice: Int, acc: Int): Int = {
      if (stones.isEmpty) acc
      else if (stones.forall(s => s.contains(alphas(indice)))) gem(stones.tail, indice + 1, acc + 1)
      else gem(stones.tail, indice + 1, acc)
    }

    gem(stones, 0, 0)

  }

  def gemStones(args: Array[String]) {
    val n = io.StdIn.readInt()
    var dis = io.StdIn.readLine()
    for (_ <- 2 to n) {
      val s = io.StdIn.readLine()
      dis = s.intersect(dis)
    }
    println(dis.length)
  }

  def gameOfThronesI(lockDoor: String) = {
    val mapped = lockDoor.toList.groupBy(identity).mapValues(_.size)
    if (lockDoor.toList.size % 2 == 0) mapped.count(x => x._2 % 2 != 0) == 0
    else mapped.count(x => x._2 % 2 != 0) == 1
  }

  def makingAnagrams(s1: String, s2: String) = (s1.length + s2.length) - (s1.toList.intersect(s2.toList).size * 2)
}
