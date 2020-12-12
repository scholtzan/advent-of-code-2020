import scala.io.Source

val input = Source.fromFile("input-12.txt")
val instructions = input.getLines.toSeq

case class Ship(
  x: Int,
  y: Int,
  r: Int,
  wx: Int = 0,
  wy: Int = 0
)

// part 1

def rotate(current: Int, delta: Int): Int = {
  val r = (current + delta) % 360

  if (r < 0) {
    360 + r
  } else {
    r
  }
}

val ship = instructions.foldLeft(Ship(0,0,90))((acc, instruction) => {
  val movement = instruction.head
  val steps = instruction.tail.mkString.toInt

  movement match {
    case 'N' => Ship(acc.x, acc.y - steps, acc.r)
    case 'S' => Ship(acc.x, acc.y + steps, acc.r)
    case 'E' => Ship(acc.x + steps, acc.y, acc.r)
    case 'W' => Ship(acc.x - steps, acc.y, acc.r)
    case 'L' => Ship(acc.x, acc.y, rotate(acc.r, -steps))
    case 'R' => Ship(acc.x, acc.y, rotate(acc.r, steps))
    case 'F' =>
      if (acc.r == 0) {
        Ship(acc.x, acc.y - steps, acc.r)
      } else if (acc.r == 90) {
        Ship(acc.x + steps, acc.y, acc.r)
      } else if (acc.r == 180) {
        Ship(acc.x, acc.y + steps, acc.r)
      } else {
        Ship(acc.x - steps, acc.y, acc.r)
      }
  }
})

val result = math.abs(ship.x) + math.abs(ship.y)

// part 2

def rotatePoint(x: Int, y: Int, steps: Int): (Int, Int) = {
  val s = (steps / 90) % 4
  val n = if (s < 0) {
    4 + s
  } else {
    s
  }

  n match {
    case 0 => (x, y)
    case 1 => (-y, x)
    case 2 => (-x, -y)
    case 3 => (y, -x)
  }
}

val ship2 = instructions.foldLeft(Ship(0,0,90,10,-1))((acc, instruction) => {
  val movement = instruction.head
  val steps = instruction.tail.mkString.toInt

  movement match {
    case 'N' => Ship(acc.x, acc.y, acc.r, acc.wx, acc.wy - steps)
    case 'S' => Ship(acc.x, acc.y, acc.r, acc.wx, acc.wy + steps)
    case 'E' => Ship(acc.x, acc.y, acc.r, acc.wx + steps, acc.wy)
    case 'W' => Ship(acc.x, acc.y, acc.r, acc.wx - steps, acc.wy)
    case 'L' =>
      val newWp = rotatePoint(acc.wx, acc.wy, -steps)
      Ship(acc.x, acc.y, acc.r, newWp._1, newWp._2)
    case 'R' =>
      val newWp = rotatePoint(acc.wx, acc.wy, steps)
      Ship(acc.x, acc.y, acc.r, newWp._1, newWp._2)
    case 'F' =>
      Ship(acc.x + acc.wx * steps, acc.y + acc.wy * steps, acc.r, acc.wx, acc.wy)
  }
})

val result2 = math.abs(ship2.x) + math.abs(ship2.y)


input.close()