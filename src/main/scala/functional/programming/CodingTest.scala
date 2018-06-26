package functional.programming

object CodingTest {

  def polygon(a: Int, b: Int, c: Int, d: Int): Int = {
    val sum = a + b + +c + d
    if (sum / 4 == a) 2
    else if (a == c && b == d) 1
    else 0
  }

  def delta_encode(array: Array[Int]): Array[Int] = {
    def delta(list: List[Int]): List[Int] = {
      if (list.tail.isEmpty) List()
      else {
        val aa = list.tail.head - list.head
        if (aa <= -127 || aa >= 127) -128 :: aa :: delta(list.tail)
        else aa :: delta(list.tail)
      }
    }

    if (array.isEmpty) Array()
    else (array.head :: delta(array.toList)).toArray
  }

  def sort_hotels(keywords: String, hotel_ids: Array[Int], reviews: Array[String]): Array[Int] = {
    val keywordsList = keywords.split(" ")
    var map = collection.mutable.Map[Int, Int]()
    for (i <- hotel_ids.indices) {
      val h = hotel_ids(i)
      val r = reviews(i).map(x => x.toLower).split(" ")
      val total = r.count(x => keywordsList.contains(x))
      if (map.exists(x => x._1 == h)) map(h) = map(h).intValue() + total
      else map += (h -> total)
    }
    map.toList.sortWith((x, y) => x._2 > y._2).map(x => x._1).toArray
  }

  def getOneBits(n: Int): Array[Int] = {
    val a = n.toBinaryString.toList
    val x = a.count(x => x == '1')
    val z = getPosition(a)
    (x :: z).toArray
  }

  def getPosition(s: List[Char]): List[Int] = {
    def position(s: List[Char], acc: Int): List[Int] = {
      if (s.isEmpty) List()
      else if (s.head == '1') acc :: position(s.tail, acc + 1)
      else position(s.tail, acc + 1)
    }

    position(s, 1)
  }


  def closestNumbers(numbers: Array[Int]): Unit = {
    val s = numbers.sorted
    val m = if (s.length < 2) 0 else s.tail.zip(s).map(x => x._1 - x._2).min
    val list = closestNumbersList(numbers, m).grouped(2).toList
    list.foreach(x => println(x.head + " " + x(1)))
  }

  def closestNumbersList(numbers: Array[Int], diff: Int): List[Int] = {
    def close(n: List[Int]): List[Int] = {
      if (n.tail.isEmpty) List()
      else if (Math.abs(n.head - n.tail.head) == diff) List(n.head, n.tail.head) ::: close(n.tail)
      else close(n.tail)
    }

    close(numbers.toList.sorted)
  }


  def degreeOfArray(arr: Array[Int]): Int = {
    val a = arr.toList
    val b = a.groupBy(identity)
      .mapValues(_.size)
      .maxBy(x => x._2)._2

    val c = a.groupBy(identity)
      .mapValues(_.size)
      .filter(x => x._2 == b)

    val d = c.keys.toList

    var k = new scala.collection.mutable.ListBuffer[Int]()
    for (i <- d) {
      val x = a.dropWhile(x => x != i)
      val z = x.tail.takeWhile(x => x != i)
      val n = z.length + 2
      k += n
    }

    k.min
  }

}
