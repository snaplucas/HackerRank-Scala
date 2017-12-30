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

