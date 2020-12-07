import scala.collection.mutable
import scala.io.Source

val input = Source.fromFile("input-07.txt")
val rules = input.getLines.toSeq

// part 1

val bags: mutable.HashMap[String, (List[String])] = mutable.HashMap()

rules.foreach { rule =>
  val parts = rule.split(" contain ")
  val outerBag = parts.head.split(" ").init.mkString(" ")

  val innerBags = parts.last.split(", ").map(bag => bag.split(" ")(1) + " " + bag.split(" ")(2))

  innerBags.foreach { innerBag =>
    if (innerBag != "other bag") {
      if (bags.contains(innerBag)) {
        bags.addOne((innerBag, (bags(innerBag) :+ outerBag).distinct))

      } else {
        bags.addOne((innerBag, List(outerBag)))
      }
    }
  }
}

def findOuterBags(bag: String): List[String] = {
  bags(bag) ++ bags(bag).flatMap{ b =>
    if (bags.contains(b)) {
      findOuterBags(b)
    } else {
      List()
    }
  }
}

val result = findOuterBags("shiny gold").distinct.length

// part 2

val bags2: mutable.HashMap[String, (List[String])] = mutable.HashMap()

rules.foreach { rule =>
  val parts = rule.split(" contain ")
  val outerBag = parts.head.split(" ").init.mkString(" ")

  val innerBags = parts.last.split(", ").map(bag => bag.split(" ").init.mkString(" ")).filter(_ != "no other")

  bags2.addOne((outerBag, innerBags.toList))
}


def countInnerBags(bag: String): Int = {
  bags2(bag).map(_.split(" ").head.toInt).sum + bags2(bag).map(b => b.split(" ").head.toInt * countInnerBags(b.split(" ").tail.mkString(" "))).sum
}

val result2 = countInnerBags("shiny gold")

input.close()
