import scala.annotation.tailrec
import scala.io.Source

val input = Source.fromFile("input-04.txt")
val passports = input.mkString.split("\n\n")

// part 1

val requiredFields = Seq("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

val validPassports = passports.flatMap(passport =>
  if (requiredFields.forall(field => passport.contains(field))) {
    Some(passport)
  } else {
    None
  }
)

validPassports.length

// part 2

val validPassports2 = validPassports.count{ passport =>
  val fields = passport.split(" ").flatMap(x => x.split("\n"))

  fields.forall { field =>
    val splitField = field.split(":")
    val name = splitField(0)
    val value = splitField(1)

    (name == "byr" && value.toInt >= 1920 && value.toInt <= 2002) ||
      (name == "iyr" && value.toInt >= 2010 && value.toInt <= 2020) ||
      (name == "eyr" && value.toInt >= 2020 && value.toInt <= 2030) ||
      (name == "hgt" && ((value.endsWith("cm") && value.split("cm")(0).toInt >= 150) && value.split("cm")(0).toInt <= 193) || ((value.endsWith("in") && value.split("in")(0).toInt >= 59) && value.split("in")(0).toInt <= 76)) ||
      (name == "hcl" && value.matches("#[0-9a-f]{6}")) ||
      (name == "ecl" && value.matches("(amb|blu|brn|gry|grn|hzl|oth)")) ||
      (name == "pid" && value.matches("[0-9]{9}")) ||
      (name == "cid")
  }
}

input.close()
