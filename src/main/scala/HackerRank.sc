val s = "asdfqwertyuighjkzxcvasdfqwertyuighjkzxcvasdfqwertyuighjkzxcvasdfqwertyuighjkzxcvasdfqwertyuighjkzxcv"
val t = "asdfqwertyuighjkzxcvasdfqwertyuighjkzxcvasdfqwertyuighjkzxcvasdfqwertyuighjkzxcvasdfqwertyuighjkzxcv"

val k = 20

s.toList.equals(t.toList)

app1(s,t, k)

def app1(s: String, t: String, k: Int): String = {
  if (s.toList.equals(t.toList) && ((k - s.length) > s.length)
  ||s.toList.equals(t.toList) && k ==s.length ) "Yes"
  else "No"
}


app1(s,t,k)
blah(s.toList,t.toList, "")


def blah(s: List[Char], t: List[Char], result: String): Boolean = {
  val first = foo(s, t, "")
  val removed = s.length - first.length
  val second = k - removed
  first.length + second >= t.length
}

def foo(s: List[Char], t: List[Char], result: String): String = {
  if (s.isEmpty || t.isEmpty || s.head != t.head) result
  else foo(s.tail, t.tail, result + s.head.toString)
}


def appendAndDelete(s: String, t: String, k: Int): String = {
  if (s.toList.equals(t.toList) && ((k - s.length) > s.length)
    || s.toList.equals(t.toList) && k ==s.length  || blah(s.toList, t.toList, ""))  "Yes"
  else "No"
}