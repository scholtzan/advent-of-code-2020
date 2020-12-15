import scala.collection.mutable

// part 1

def recite(n: Int, start: String): Long = {
  val input = start.split(",").map(_.toInt)
  val spokenNumbers: mutable.HashMap[Long, Long] = mutable.HashMap()

  input.init.zipWithIndex.foreach { case (number, index) =>
    spokenNumbers.addOne(number, index)
  }

  (input.length until n).foldLeft(input.last.toLong) { case (acc, i) =>
    if (!spokenNumbers.contains(acc)) {
      spokenNumbers.addOne(acc, i - 1)
      0.toLong
    } else {
      val last = i.toLong - 1 - spokenNumbers(acc)
      spokenNumbers.addOne(acc, i - 1)
      last
    }
  }
}

val result = recite(2020, "12,1,16,3,11,0")

// part 2

val result = recite(30000000, "12,1,16,3,11,0")
