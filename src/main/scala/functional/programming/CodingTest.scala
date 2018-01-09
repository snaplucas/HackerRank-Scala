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
    val another = map.toList.sortWith((x, y) => x._2 > y._2)
    another.map(x => x._1).toArray
  }

}
