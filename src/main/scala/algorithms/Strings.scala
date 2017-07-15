package algorithms

object Strings {

  def main(args: Array[String]) {

    println(generateList("91011".toList))
    //    sherlockAndTheValidString_2("aabbccddeefghi")
    //    sherlockAndTheValidString_2("aaaa")
    //    sherlockAndTheValidString_2("abdc")
    //
    //    val sherlock = "aabbccddeefghi".toList.groupBy(identity).mapValues(_.size).groupBy(x => x._2).mapValues(_.size)
    //    println(sherlock)
    //    println(sherlockAndTheValidString("hfchdkkbfifgbgebfaahijchgeeeiagkadjfcbekbdaifchkjfejckbiiihegacfbchdihkgbkbddgaefhkdgccjejjaajgijdkd"))
    //
    //    println(anagram("aaabbb"))
    //
    //    println(theLoveletterMystery("heubsbn"))
    //
    //    println(makingAnagrams("cde", "abc"))
    //
    //    println(gameOfThronesI("aabbcc"))
    //
    //    println(if (hackerRankInAString("hacakaeararanaka".toList)) "YES" else "NO")
    //    println(if (hackerRankInAString("hhhhaaaaackkkkerrrrrrrrank".toList)) "YES" else "NO")
    //    println(if (hackerRankInAString("crackerhackerknar".toList)) "YES" else "NO")
    //
    //    println(gemStones(List("abcd".toList, "abef".toList, "abjj".toList)))
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

  def theLoveletterMystery(s: String) = {
    def love(s: String, acc: Int): Int =
      if (s.isEmpty) acc
      else love(s.drop(1).dropRight(1), acc + Math.abs(s.head - s.reverse.head))

    love(s, 0)
  }

  def anagram(s: String) =
    if (s.length % 2 != 0) -1
    else {
      val s1 = s.slice(0, s.length / 2)
      val s2 = s.slice(s.length / 2, s.length)
      ((s1.length + s2.length) - (s1.toList.intersect(s2.toList).size * 2)) / 2
    }

  def twoStrings(s1: String, s2: String) = println(if (s1.toList.intersect(s2.toList).nonEmpty) "YES" else "NO")

  def sherlockAndTheValidString(s: String) = s.toList.groupBy(identity).mapValues(_.size).groupBy(x => x._2).mapValues(_.size).size <= 2

  def sherlockAndTheValidString_2(s: String) = {
    val s1 = s.toList.groupBy(identity).mapValues(_.size)
    val s2 = s1.groupBy(x => x._2).mapValues(_.size)
    s2.size == 1 || (s1.size <= 2 && s2.map(x => x._1 == 1).size <= 1)
  }

  //   val n = s.toList.map(_.toInt)

  def generateList(s: List[Char]): List[Char] = s match {
    case List() => List()
    case x1 :: x2 :: y => if (x2 != 0 && x2 > x1) x1 :: x2 :: generateList(y) else x1 :: generateList(x2 :: y)
  }

  def separateTheNumbers(s: List[Int]) = for {i <- 1 until s.size
                                              j <- 1 until i
                                              if j - i == 1} yield true
}