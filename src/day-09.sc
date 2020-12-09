import scala.annotation.tailrec
import scala.io.Source

val input = Source.fromFile("input-09.txt")
val numbers = input.getLines.toSeq.map(_.toLong)

// part 1

val n = 25
val result = numbers.sliding(n + 1).find { n =>
  !n.init.combinations(2).exists(s => s.head + s.last == n.last && s.head != s.last)
}.get.last

// part 2

@tailrec
def findContiguous(con: Seq[Long], index: Int): Long = {
  if (con.sum == result) {
    return con.min + con.max
  }

  if (con.sum < result) {
    findContiguous(con :+ numbers(index), index + 1)
  } else {
    findContiguous(con.tail, index)
  }
}

val result2 = findContiguous(Seq(), 0)

input.close()