import scala.io.Source

val input = Source.fromFile("input-01.txt")
val expenses = input.getLines.toSeq.map(l => l.toInt)

// part 1

val expense = expenses.find(e => expenses.contains(2020 - e)).get
val result = expense * (2020 - expense)

// part 2

var expenses2 = (0, 0, 0)

expenses.foreach(e =>
  expenses.foreach(e2 =>
    if (expenses.contains(2020 - e - e2)) {
      expenses2 = (e, e2, 2020 - e - e2)
    }
  )
)

// non mutable

val r = expenses.groupBy(e => expenses.groupBy(x => expenses.find(z => z == 2020 - e - x)).filter(a => a._1.isDefined)).filter(a => a._1.nonEmpty).head
val result2 = r._2.head * r._1.head._1.get * r._1.last._1.get

input.close()
