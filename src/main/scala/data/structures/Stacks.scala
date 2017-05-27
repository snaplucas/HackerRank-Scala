package data.structures

object Stacks {

  def main(args: Array[String]) {
    val s = "{{[[(())]]}}"
    if ((balancedBrackets(s.toList, '(', ')')
      && (balancedBrackets(s.toList, '[', ']')
      && balancedBrackets(s.toList, '{', '}')))) println("YES") else println("NO")
  }

  def balancedBrackets(chars: List[Char], first: Char, last: Char): Boolean = {
    def balanced(chars: List[Char], open: Int): Boolean =
      if (chars.isEmpty) open == 0
      else if (chars.head == first) balanced(chars.tail, open + 1)
      else if (chars.head == last) open > 0 && balanced(chars.tail, open - 1)
      else balanced(chars.tail, open)

    balanced(chars, 0)
  }

}
