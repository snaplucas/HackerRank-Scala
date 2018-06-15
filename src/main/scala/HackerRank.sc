val a = List(1, 2, 3, 4, 5)
val (b,c) = a.splitAt(4)

(c ++ b)mkString " "