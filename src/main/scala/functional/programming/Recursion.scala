package functional.programming

object Recursion {

  def main(args: Array[String]): Unit = {

    println(stringCompression("aaabaaaaccaaaaba".toList, 0))

    //    println(fibonacci(5))
    //    println(stringMinglingV2("abcde", "pqrst"))
    //    println(stringPermute("abcdpqrs".toList))
  }

  def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)

  def fibonacci(x: Int): Int = {
    if (x == 1) 0
    else if (x == 2) 1
    else fibonacci(x - 1) + fibonacci(x - 2)
  }

  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r == c) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)

  def stringMingling(p: String, q: String, acc: String = ""): String = {
    if (p.isEmpty) acc
    else stringMingling(p.tail, q.tail, acc + p.head + q.head)
  }

  def stringMinglingV2(p: String, q: String) = p.toList.zip(q.toList).flatMap(x => x._1 + "" + x._2).mkString

  def stringPermute(s: List[Char]): String = s match {
    case Nil => ""
    case xa :: xb :: x => xb + "" + xa + stringPermute(x)
  }

  def stringPermuteV2(input: Vector[Char], res: Vector[Char]): String =
    if (input.length >= 2) stringPermuteV2(input.drop(2), res ++ input.take(2).reverse)
    else res.mkString("")

  def stringCompression(msg: List[Char], acc: Int): String = msg match {
    case List(x) =>
      if (acc != 0) x.toString + (acc + 1).toString
      else x.toString
    case xa :: xb :: x =>
      if (xa == xb) stringCompression(xb :: x, acc + 1)
      else if (acc != 0) xa + (acc + 1).toString + stringCompression(xb :: x, 0)
      else xa + stringCompression(xb :: x, 0)
  }
}