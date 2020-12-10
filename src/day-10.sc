import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

val input = Source.fromFile("input-10.txt")
val adapters = input.getLines.toSeq.map(_.toInt).sorted

// part 1

val joltDifferences = adapters.zipWithIndex.map { case (jolt, index) =>
  if (index == 0) {
    jolt - 0
  } else {
    adapters(index) - adapters(index - 1)
  }
}

val result = (joltDifferences.count(_ == 3) + 1) * joltDifferences.count(_ == 1)

// part 2

var numberOfWays: mutable.HashMap[Int, Long] = mutable.HashMap()
numberOfWays.addOne(0, 1.toLong)

val deviceJolt = adapters.max + 3

(adapters :+ deviceJolt).foreach { adapter =>
  val ways = (adapter - 3).until(adapter).map { a =>
    if (numberOfWays.contains(a)) {
      numberOfWays(a)
    } else {
      0.toLong
    }
  }.sum

  numberOfWays.addOne(adapter, ways)
}

val result2 = numberOfWays(deviceJolt)

input.close()