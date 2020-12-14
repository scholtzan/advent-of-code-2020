import scala.collection.mutable
import scala.io.Source

val input = Source.fromFile("input-14.txt")
val program = input.getLines.toSeq

val memory: mutable.HashMap[Long, Long] = mutable.HashMap()

// part 1

def applyMask(value: Long, mask: String) = {
  val binary = value.toBinaryString
  val placeholders = "0" * (mask.length - binary.length)
  val masked = (placeholders + binary).zipWithIndex.map { case (c, index) =>
    if (mask(index) != 'X') {
      mask(index)
    } else {
      c
    }
  }.mkString

  java.lang.Long.parseLong(masked, 2)
}

program.foldLeft("") { case (mask, instruction) =>
  if (instruction.startsWith("mask")) {
    instruction.split("mask = ").last
  } else {
    val pattern = """mem.(\d+). = (.+)""".r
    val (mem, value) = pattern.findAllIn(instruction).matchData.map(m => (m.group(1).toLong, m.group(2).toLong)).toSeq.head

    val maskedValue = applyMask(value, mask)
    memory.addOne(mem, maskedValue)
    mask
  }
}

val result = memory.values.sum

// part 2

val memory2: mutable.HashMap[Long, Long] = mutable.HashMap()

def recApplyMask2(value: String, mask: String): Seq[String] = {
  if (value.isEmpty) {
    return Seq("")
  }

  if (mask.head == '0') {
    recApplyMask2(value.tail, mask.tail).map(x => value.head + x)
  } else if (mask.head == '1') {
    recApplyMask2(value.tail, mask.tail).map(x => '1' + x)
  } else {
    recApplyMask2(value.tail, mask.tail).map(x => '1' + x) ++
    recApplyMask2(value.tail, mask.tail).map(x => '0' + x)
  }
}

def applyMask2(value: Long, mask: String): Seq[Long] = {
  val binary = value.toBinaryString
  val placeholders = "0" * (mask.length - binary.length)
  val values = recApplyMask2(placeholders + binary, mask)
  values.map(v => java.lang.Long.parseLong(v, 2))
}

program.foldLeft("") { case (mask, instruction) =>
  if (instruction.startsWith("mask")) {
    instruction.split("mask = ").last
  } else {
    val pattern = """mem.(\d+). = (.+)""".r
    val (mem, value) = pattern.findAllIn(instruction).matchData.map(m => (m.group(1).toLong, m.group(2).toLong)).toSeq.head

    val maskedValues = applyMask2(mem, mask)
    maskedValues.foreach { m =>
      memory2.addOne(m, value)
    }

    mask
  }
}

val result2 = memory2.values.sum


input.close()