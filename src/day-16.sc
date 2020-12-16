import scala.io.Source

val input = Source.fromFile("input-16.txt")
val ticket_info = input.getLines.toSeq

// part 1

val valid_fields = ticket_info.takeWhile(_ != "")
val valid_ranges = valid_fields.flatMap { field =>
  val ranges = field.split(": ").last
  ranges.split(" or ").flatMap(r => r.split("-").head.toInt to r.split("-").last.toInt)
}

val other_tickets = ticket_info.reverse.takeWhile(_ != "nearby tickets:")

val result = other_tickets.flatMap { ticket =>
  ticket.split(",").filter(n => !valid_ranges.contains(n.toInt))
}.map(_.toInt).sum


// part 2

val field_ranges = valid_fields.map { field =>
  val ranges = field.split(": ").last
  ranges.split(" or ").flatMap(r => r.split("-").head.toInt to r.split("-").last.toInt)
}

val filtered_values_by_field = other_tickets.filter(t => t.split(",").map(_.toInt).forall(valid_ranges.contains))
  .flatMap(t => t.split(",").zipWithIndex).groupBy(_._2).map(x => x._1 -> x._2.map(_._1.toInt))

val possible_fields = field_ranges.zipWithIndex.flatMap { range =>
  val v = filtered_values_by_field.filter(fv => fv._2.forall(range._1.contains))
    v.keys.map { x =>
    (range._2, x)
  }.toSeq.distinct
}

def resolve(possibilties: Seq[(Int, Int)]): Seq[(Int, Int)] = {
  if (possibilties.isEmpty) {
    return Seq()
  }

  val valid = possibilties.find(p => possibilties.count(x => x._1 == p._1) == 1).get

  (valid._1, valid._2) +: resolve(possibilties.filter(p => p._1 != valid._1 && p._2 != valid._2))
}

val field_names = ticket_info.takeWhile(_ != "").map(_.split(": ").head)
val field_name_assignments = resolve(possible_fields)
val relevant_columns = field_name_assignments.filter(a => field_names(a._1).startsWith("departure"))

val your_ticket = ticket_info.takeWhile(_ != "nearby tickets:").init.last.split(",").map(_.toLong)
val result2 = relevant_columns.map(c => your_ticket(c._2)).product



