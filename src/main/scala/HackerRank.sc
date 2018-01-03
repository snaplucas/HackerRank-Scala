val s = "hfchdkkbfifgbgebfaahijchgeeeiagkadjfcbekbdaifchkjfejckbiiihegacfbchdihkgbkbddgaefhkdgccjejjaajgijdkd"

val sherlock_1 = s.toList.groupBy(identity)
  .mapValues(_.size)
  .groupBy(x => x._2)
  .mapValues(_.size)

val sherlock_2 = s.toList.groupBy(identity)
  .mapValues(_.size)
  .count(x => x._2 == 1) == 1
//  .groupBy(x => x._2)
//  .mapValues(_.size)
//    .count(y => y._2 == 1) == 1
//  .exists(y => y._2 == 1)