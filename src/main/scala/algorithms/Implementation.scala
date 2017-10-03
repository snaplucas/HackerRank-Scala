package algorithms

object Implementation {

  def main(args: Array[String]): Unit = {
    println(taumAndBday(3, 3, 1, 9, 2))

  }

  def taumAndBday(b: Long, w: Long, x: Long, y: Long, z: Long): Long = {
    val yy = if (x + z <= y) x + z else y
    val xx = if (y + z <= x) y + z else x
    b * xx + w * yy
  }

  def dayOfTheProgrammer(year: Int): Unit = {
    val bi = if (year < 1917) year % 4 == 0 else year % 400 == 0 || (year % 4 == 0 && !(year % 100 == 0))
    if (year == 1918) println("26.09." + year)
    else if (bi) println("12.09." + year)
    else println("13.09." + year)
  }
}
