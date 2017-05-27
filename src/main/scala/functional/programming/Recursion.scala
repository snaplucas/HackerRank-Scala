package functional.programming

object Recursion {

  def main(args: Array[String]): Unit = {
    println(fibonacci(5))
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
}
