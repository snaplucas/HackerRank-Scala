package algorithms

object Strings {

  def main(args: Array[String]) {

    //    println(if (hackerRankInAString("hacakaeararanaka".toList)) "YES" else "NO")
    //    println(if (hackerRankInAString("hhhhaaaaackkkkerrrrrrrrank".toList)) "YES" else "NO")
    //    println(if (hackerRankInAString("crackerhackerknar".toList)) "YES" else "NO")

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

  def gemStones(stones: List[List[Char]]) =
    for {stone <- stones
         gem <- stone
         if stones.forall(a => a.forall(b => b == gem))} yield gem

  def makingAnagrams(a: String, b: String) = {}
}
