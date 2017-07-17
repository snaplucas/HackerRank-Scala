package algorithms

object Implementation {

  def main(args: Array[String]): Unit = {
    println(taumAndBday(3, 3, 1, 9, 2))

  }

  def taumAndBday(b: Long, w: Long, x: Long, y: Long, z: Long) = {
    val yy = if (x + z <= y) x + z else y
    val xx = if (y + z <= x) y + z else x
    b * xx + w * yy
  }
}
