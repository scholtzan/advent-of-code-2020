import scala.annotation.tailrec
import scala.io.Source

val input = Source.fromFile("input-03.txt")
val field = input.getLines.toSeq

// part 1

@tailrec
def countTrees(totalTrees: Int, x: Int, y: Int, xStep: Int, yStep: Int): BigInt = {
  if (y + 1 >= field.length) {
    return totalTrees
  }

  val newX = if (x + xStep >= field.head.length) {
    xStep - (field.head.length - x)
  } else {
    x + xStep
  }

  val cell = field(y + yStep)(newX)

  if (cell == '#') {
    countTrees(totalTrees + 1, newX, y + yStep, xStep, yStep)
  } else {
    countTrees(totalTrees, newX, y + yStep, xStep, yStep)
  }
}

val result: BigInt = countTrees(0, 0, 0, 3, 1)

// part 2


val result2: BigInt = result * countTrees(0, 0, 0, 1, 1) * countTrees(0, 0, 0, 5, 1) * countTrees(0, 0, 0, 7, 1) * countTrees(0, 0, 0, 1, 2)

input.close()
