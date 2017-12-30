package algorithms

object Implementation {

  def main(args: Array[String]): Unit = {
    //    println(chocolateFeast(43203, 60, 5))
    //
    //    println(taumAndBday(3, 3, 1, 9, 2))
    //    val rounded = gradingStudents(List(73, 67, 38, 33))
    //    rounded.foreach(x => println(x))

    //
    //    val lista = List(10, 20, 30, 40, 50)
    //    lista.filter(x => x > 20).map(y => y * 2).foreach(z => println(z))

    //    println(betweenTwoSets(Array(1), Array(100)))
    //    println(migratoryBirds(Array(1, 4, 4, 4, 5, 3)))
    println(beautifulTriplets(3, List(1, 2, 4, 5, 7, 8, 10)))

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

  def chocolateFeast(money: Int, cost: Int, extra: Int): Int = {
    val boughtCandies = money / cost
    boughtCandies + (boughtCandies - 1) / (extra - 1)
  }

  def gradingStudents(grades: List[Int]): List[Int] = grades.map(x => if (x >= 38 && (5 - x % 5) < 3) x + (5 - x % 5) else x)

  def betweenTwoSets(a: Array[Int], b: Array[Int]): Int = {
    val factors = (for {x <- 1 until 101 if b.forall(y => y % x == 0)} yield x).toSet
    (for {z <- factors if a.forall(x => z % x == 0)} yield z).size
  }

  def migratoryBirds(birds: Array[Int]): Int = {
    birds.groupBy(identity).maxBy(x => x._2.length)._1
  }

  def countingValleys(s: String): Int = {
    def count(s: List[Char], up: Int, v: Int): Int = {
      if (s.isEmpty) v
      else if (s.head == 'D' && up == 0) count(s.tail, up - 1, v + 1)
      else if (s.head == 'D') count(s.tail, up - 1, v)
      else count(s.tail, up + 1, v)
    }

    count(s.toList, 0, 0)
  }

  def beautifulTriplets(d: Int, arr: List[Int]): Int = {
    def beautiful(list: List[Int], acc: Int): Int = {
      if (list.isEmpty) acc
      else {
        var number = list.head
        var tri = 0
        for (x <- list.tail) {
          if (number - x == d) {
            number = x
            tri = tri + 1
          }
        }
        val aux = if (tri > 1) 1 else 0
        beautiful(list.tail, acc + aux)
      }
    }

    beautiful(arr.reverse, 0)
  }

}
