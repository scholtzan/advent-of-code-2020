import scala.collection.mutable
import scala.io.Source

val input = Source.fromFile("input-18.txt")
val equations = input.getLines.toSeq

// part 1

def evaluate(parts: Seq[String]): Long = {
  if (parts.isEmpty) {
    return 0
  }

  if (parts.last == ")") {
    val inner = parts.zipWithIndex.reverse.find { case (part, index) =>
      part == "(" && parts.slice(index + 1, parts.length - 1).count(_ == "(") == parts.slice(index + 1, parts.length - 1).count(_ == ")")
    }

    val innerEquation = parts.slice(inner.get._2 + 1, parts.length - 1)

    if (parts.length > innerEquation.length + 2) {
      if (parts(parts.length - innerEquation.length - 3) == "+") {
        evaluate(parts.slice(0, parts.length - innerEquation.length - 3)) + evaluate(innerEquation)
      } else {
        evaluate(parts.slice(0, parts.length - innerEquation.length - 3)) * evaluate(innerEquation)
      }
    } else {
      evaluate(innerEquation)
    }
  } else {
    val num = parts.reverse.takeWhile(s => s != "+" && s != "*" && s != "(" && s != ")").reverse.mkString
    val intNum = num.toLong

    if (parts.length == num.length) {
      intNum
    } else {
      if (parts(parts.length - num.length - 1) == "+") {
        evaluate(parts.slice(0, parts.length - num.length - 1)) + intNum
      } else {
        evaluate(parts.slice(0, parts.length - num.length - 1)) * intNum
      }
    }
  }
}

val result = equations.map { e =>
  evaluate(e.split(" ").mkString.split("").toSeq)
}.sum

// part 2



input.close()