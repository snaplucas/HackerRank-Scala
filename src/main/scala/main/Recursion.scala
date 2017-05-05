package main

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
}
