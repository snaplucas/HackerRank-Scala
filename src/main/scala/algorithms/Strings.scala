package algorithms

object Strings {

  def main(args: Array[String]) {
    //        println(theLoveletterMystery("heubsbn"))
    //
    //        println(makingAnagrams("cde", "abc"))
    //
    //        println(gameOfThronesI("aabbcc"))
    //
    //        println(if (hackerRankInAString("hacakaeararanaka".toList)) "YES" else "NO")
    //        println(if (hackerRankInAString("hhhhaaaaackkkkerrrrrrrrank".toList)) "YES" else "NO")
    //        println(if (hackerRankInAString("crackerhackerknar".toList)) "YES" else "NO")
    //
    //        println(gemStones(List("abcd".toList, "abef".toList, "abjj".toList)))
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

  def anagram(s: String) = if (s.length % 2 != 0) -1
  else {
    val s1 = s.slice(0, s.length / 2)
    val s2 = s.slice(s.length / 2, s.length)
    ((s1.length + s2.length) - (s1.toList.intersect(s2.toList).size * 2)) / 2
  }

  def twoStrings(s1: String, s2: String) = println(if (s1.toList.intersect(s2.toList).nonEmpty) "YES" else "NO")

  def sherlockAndTheValidString(s: String): Unit = {
    val sherlock_1 = s.toList.groupBy(identity)
      .mapValues(_.size)
      .groupBy(x => x._2)
      .mapValues(_.size).size == 1

    val sherlock_2 = s.toList.groupBy(identity)
      .mapValues(_.size)
      .count(x => x._2 == 1) == 1

    if (sherlock_1 || sherlock_2) println("YES") else println("NO")
  }

  def separateTheNumbers(s: String): Option[String] = {
    for (i <- 1 to s.length / 2) {
      if (s.take(i)(0) == '0') None

      if (checkSequence(s.take(i), s.drop(i))) Some(s.take(i))
    }
    None
  }

  def checkSequence(prev: String, rest: String): Boolean = {
    if (rest == "") true
    else if (rest.length < prev.length) false
    else if (rest.take(prev.length).toLong - prev.toLong == 1) checkSequence(rest.take(prev.length), rest.drop(prev.length))
    else if (rest.length > prev.length && rest.take(prev.length + 1).toLong - prev.toLong == 1) checkSequence(rest.take(prev.length + 1), rest.drop(prev.length + 1))
    else false
  }

  //TODO: fix
  def weightUniformString(s: String, n: Int): Unit = {
    val englishLetters = ('a' to 'z').toList.sorted
    val uniforms = s.toList.groupBy(identity).mapValues(_.size)
    val weightsMap = uniforms.map(x => x._2 * englishLetters.indexOf(x._1 + 1))
    weightsMap.exists(x => x % n == 0)
  }

  //TODO: fix
  def bigSorting(arr: Array[String]): Array[String] = {
    val first = arr.map(x => x.toInt).sorted
    first.map(x => x.toString)
  }

  def encryption(s: String): String = {
    def blah(s: String, t: List[List[Char]]): String = {
      if (t.exists(x => x.nonEmpty))
        blah(s + " " + t.filter(y => y.nonEmpty).map(x => x.head).mkString, t.filter(y => y.nonEmpty).map(y => y.tail))
      else s
    }

    val k = scala.math.sqrt(s.length).ceil.toInt
    val t = s.toList.grouped(k).toList
    blah("", t).trim
  }
}