val s = "hackerhappy"
val t = "hackerrank"

val intersect = (s zip t).takeWhile(x => x._1 == x._2).map(_._1).mkString
