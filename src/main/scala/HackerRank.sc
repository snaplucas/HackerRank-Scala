import scala.collection.BitSet
//val s = "abccddde"
//
//val englishLetters = ('a' to 'z').toList.sorted
//
//val uniforms = s.toList.groupBy(identity).mapValues(_.size)
//
//val weightsMap = uniforms.map(x => x._2 * englishLetters.indexOf(x._1 + 1))
//
//weightsMap.exists(x => x % 8 == 0)

//def isPalindrome(s: List[Char]): Boolean = s match {
//  case Nil => true
//  case List(_) => true
//  case list => list.head == list.last && isPalindrome(list.tail.init)
//}
//
//def palindromeIndex(s: String): Int = {
//  def palindrome(s: List[Char], index: Int): Int = {
//    if (isPalindrome(s)) index
//    else palindrome(s, index + 1)
//  }
//
//  palindrome(s.toList, -1)
//}

//"hackerhappy".toList.intersect("hackerrank".toList)

//"10101".toList.zip("11010").count(x => x._1 == '1' || x._2 == '1')

"10101".toList

val list = Array("10101", "11100", "11010", "00101").toList

val pairs = list.tails.flatMap {
  case x :: rest => rest.map(y => x.toList.zip(y))
      .map(y => y.count(x => x._1 == '1' || x._2 == '1'))
  case _ => List()
}.toList

pairs.max
pairs.count(x => x == pairs.max)

