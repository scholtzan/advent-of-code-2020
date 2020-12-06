import scala.io.Source

val input = Source.fromFile("input-06.txt")
val groupAnswers = input.mkString.split("\n\n")

// part 1

val totalAnswers = groupAnswers.map { group =>
  group.split("\n").flatMap(a => a.split("")).distinct.length
}

val result = totalAnswers.sum

// part 2

val totalAnswers2 = groupAnswers.map { group =>
  group.split("\n").reduce((identical, a) => identical.intersect(a)).length
}

val result2 = totalAnswers2.sum


input.close()
