import scala.io.Source

val input = Source.fromFile("input-05.txt")
val passes = input.getLines.toSeq

// part 1

val seats = passes.map { pass =>
  val row = Integer.parseInt(pass.take(7).map {
    case 'F' => '0'
    case 'B' => '1'
  }.mkString, 2)

  val column = Integer.parseInt(pass.takeRight(3).map {
    case 'R' => '1'
    case 'L' => '0'
  }.mkString, 2)

  row * 8 + column
}

seats.max

// part 2

seats.filter(s => !seats.contains(s - 1))

input.close()
