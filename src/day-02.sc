import scala.io.Source

val input = Source.fromFile("input-02.txt")
val passwords = input.getLines.toSeq

// part 1
val validPasswords = passwords.flatMap { password =>
  val parts = password.split(" ")
  val range = parts(0).split("-")
  val min = range(0).toInt
  val max = range(1).toInt
  val letter = parts(1).split(":").head
  val pwd = parts(2)
  val occurrences = pwd.count(l => l == letter.toCharArray.head)

  if (occurrences >= min && occurrences <= max) {
    Some(password)
  } else {
    None
  }
}

val result = validPasswords.length

// part 2

val validPasswords2 = passwords.flatMap { password =>
  val parts = password.split(" ")
  val range = parts(0).split("-")
  val min = range(0).toInt - 1
  val max = range(1).toInt - 1
  val letter = parts(1).split(":").head.head
  val pwd = parts(2)

  if ((pwd(min) == letter && pwd(max) != letter) || (pwd(min) != letter && pwd(max) == letter)) {
    Some(password)
  } else {
    None
  }
}

val result2 = validPasswords2.length

input.close()
