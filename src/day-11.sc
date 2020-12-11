import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

val input = Source.fromFile("input-11.txt")
val seatMap = input.getLines.toSeq.map(_.split("").toSeq)

// part 1

def totalOccupiedAdjacentSeats(seats: Seq[Seq[String]], row: Int, column: Int): Int = {
  (-1 to 1).map { dr =>
    (-1 to 1).map { dc =>
      if (row + dr >= 0 && row + dr < seats.length &&
      column + dc >= 0 && column + dc < seats.head.length) {
        if (seats(row + dr)(column + dc) == "#" && !(dc == 0 && dr == 0)) {
          1
        } else {
          0
        }
      } else {
        0
      }
    }.sum
  }.sum
}

@tailrec
def occupiedSeats(seats: Seq[Seq[String]]): Int = {
  val newSeatMap = seats.zipWithIndex.map { case (seatRow, row) =>
    seatRow.zipWithIndex.map { case (seat, column) =>
      val adjacentOccupied = totalOccupiedAdjacentSeats(seats, row, column)
      seat match {
        case "." => "."
        case "#" =>
          if (adjacentOccupied >= 4) {
            "L"
          } else {
            "#"
          }
        case "L" =>
          if (adjacentOccupied == 0) {
            "#"
          } else {
            "L"
          }
      }
    }
  }

  if (newSeatMap == seats) {
    newSeatMap.map(_.count(_ == "#")).sum
  } else {
    occupiedSeats(newSeatMap)
  }
}

//val result = occupiedSeats(seatMap)

// part 2

def totalOccupiedAdjacentSeats2(seats: Seq[Seq[String]], row: Int, column: Int): Int = {
  (-1 to 1).map { dr =>
    (-1 to 1).map { dc =>
      var step = 1
      while (row + dr * step >= 0 && row + dr * step < seats.length &&
        column + dc * step >= 0 && column + dc * step < seats.head.length &&
        seats(row + dr * step)(column + dc * step) == "." && !(dc * step == 0 && dr * step == 0)) {
        step += 1
      }

      if (row + dr * step >= 0 && row + dr * step < seats.length &&
        column + dc * step >= 0 && column + dc * step < seats.head.length) {
        if (seats(row + dr * step)(column + dc * step) == "#" && !(dc * step == 0 && dr * step == 0)) {
          1
        } else {
          0
        }
      } else {
        0
      }
    }.sum
  }.sum
}

@tailrec
def occupiedSeats2(seats: Seq[Seq[String]]): Int = {
  val newSeatMap = seats.zipWithIndex.map { case (seatRow, row) =>
    seatRow.zipWithIndex.map { case (seat, column) =>
      val adjacentOccupied = totalOccupiedAdjacentSeats2(seats, row, column)
      seat match {
        case "." => "."
        case "#" =>
          if (adjacentOccupied >= 5) {
            "L"
          } else {
            "#"
          }
        case "L" =>
          if (adjacentOccupied == 0) {
            "#"
          } else {
            "L"
          }
      }
    }
  }

  if (newSeatMap == seats) {
    newSeatMap.map(_.count(_ == "#")).sum
  } else {
    occupiedSeats2(newSeatMap)
  }
}


val result2 = occupiedSeats2(seatMap)

input.close()