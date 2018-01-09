val s = "breakfast beach".split(" ")
val t = "breakfast breakfast beach beach".split(" ")

val n = t.count(x => s.contains(x))
