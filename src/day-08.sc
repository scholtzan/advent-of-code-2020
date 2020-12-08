import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

val input = Source.fromFile("input-08.txt")

val instructions: Seq[(String, Int)] = input.getLines.map{ instruction =>
  val instructionParts = instruction.split(" ")
  val command = instructionParts(0)
  val arg = instructionParts(1).toInt

  (command, arg)
}.toSeq

// part 1

def execute(i: Int, acc: Int, instructionHistory: Array[Int], modify: Boolean): (Int, Boolean) = {
  if (i >= instructions.length) {
    return (acc, true)
  }

  if (instructionHistory.contains(i)) {
    return (acc, false)
  }

  val instruction = instructions(i)

  instruction._1 match {
    case "acc" => execute(i + 1, acc + instruction._2, instructionHistory :+ i, modify)
    case "jmp" =>
      if (modify) {
        val res = execute(i + 1, acc, instructionHistory :+ i, modify = false)

        if (res._2) {
          (res._1, true)
        } else {
          execute(i + instruction._2, acc, instructionHistory :+ i, modify = true)
        }
      } else {
        execute(i + instruction._2, acc, instructionHistory :+ i, modify = false)
      }
    case "nop" =>
      if (modify) {
        val res = execute(i + instruction._2, acc, instructionHistory :+ i, modify = false)

        if (res._2) {
          (res._1, true)
        } else {
          execute(i + 1, acc, instructionHistory :+ i, modify = true)
        }
      } else {
        execute(i + 1, acc, instructionHistory :+ i, modify = false)
      }
  }
}

val result = execute(0, 0, Array(), modify = false)._1

// part 2

val result2 = execute(0, 0, Array(), modify = true)._1

input.close()