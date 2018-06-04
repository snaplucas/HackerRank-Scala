def getOneBits(n: Int): Array[Int] = {
  val a = n.toBinaryString.toList
  val x = a.count(x => x == '1')
  val z = getPosition(a)
  (x :: z).toArray
}

def getPosition(s: List[Char]) = {
  def position(s: List[Char], acc: Int): List[Int] = {
    if (s.isEmpty) List()
    else if (s.head == '1') acc :: position(s.tail, acc + 1)
    else position(s.tail, acc + 1)
  }

  position(s, 1)
}


def closestNumbers(numbers: Array[Int]) = {
  val s = numbers.sorted
  val m = if (s.length < 2) 0 else s.tail.zip(s).map(x => x._1 - x._2).min
  val list = closestNumbersList(numbers, m).grouped(2).toList
  list.foreach(x => println(x.head + " "  + x(1)))
}

def closestNumbersList(numbers: Array[Int], diff: Int) = {
  def close(n: List[Int]): List[Int] = {
    if(n.tail.isEmpty) List()
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
  for(i <- d){
    val x = a.dropWhile(x => x != i)
    val z = x.tail.takeWhile(x => x != i)
    val n = z.length + 2
    k += n
  }

  k.min
}
