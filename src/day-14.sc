import scala.annotation.tailrec
import scala.io.Source

val input = Source.fromFile("input-13.txt")
val notes = input.getLines.toSeq

val arrival = notes.head.toLong
val busses = notes.last.split(",")

// part 1

val earliest = busses.filter(_ != "x").minBy { bus =>
  val busNumber = bus.toLong
  val delay = arrival % busNumber
  busNumber - delay
}.toLong

val result = earliest * (earliest - (arrival % earliest))


// part 2

@tailrec
def gcd(a: Long, b: Long): Long = {
  if (b == 0) a else gcd(b, a % b)
}

val result2 = busses.zipWithIndex.filter(_._1 != "x").tail.foldLeft((busses.head.toLong, busses.head.toLong)) { case (acc, (bus, index)) =>
  var t = acc._2
  while ((t + index) % bus.toLong != 0) {
    t += acc._1
  }

  ((acc._1 * bus.toLong) / gcd(acc._1, bus.toLong), t)
}._2


input.close()