val s = "abccddde"

val englishLetters = ('a' to 'z').toList.sorted

val uniforms = s.toList.groupBy(identity).mapValues(_.size)

val weightsMap = uniforms.map(x => x._2 * englishLetters.indexOf(x._1 + 1))

weightsMap.exists(x => x % 8 == 0)

