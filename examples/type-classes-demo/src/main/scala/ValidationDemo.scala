import cats.data.Validated._
import cats.data._
import cats.implicits._

case class Date(year: Int, month: Int, day: Int) {
  require(isValid(year, month, day))

  // TODO: Use correct implementation
  private def isValid(year: Int, month: Int, day: Int) = year > 0 && month > 0 && day > 0
}

object Date {
  def validated(year: Int, month: Int, day: Int): Option[Date] = try {
    Some(Date(year, month, day))
  } catch {
    case _: IllegalArgumentException => None
  }
}

case class Person(name: String, email: String, age: Int, birthday: Date)

case class Input(name: String, email: String, age: String,
                 birthYear: String, birthMonth: String, birthDay: String)

object ValidationTest extends App {
  def name(value: String) =
    if (!value.isEmpty) valid(value)
    else invalidNel(s"Missing user name")

  def email(value: String) =
    if (value.contains("@")) valid(value.toLowerCase)
    else invalidNel(s"$value is not a valid email")

  def integer(value: String) =
    try valid(value.toInt)
    catch {
      case _: NumberFormatException => invalidNel(s"$value is not an integer")
    }

  def nonNegativeInteger(value: String) =
    integer(value).andThen { i =>
      if (i >= 0) valid(i)
      else invalidNel(s"$i is negative")
    }

  def date(year: Int, month: Int, day: Int) = Date.validated(year, month, day) match {
    case Some(date) => valid(date)
    case None => invalidNel(s"$year/$month/$day is not a valid date")
  }

  def validate(input: Input): ValidatedNel[String, Person] = {
    val birthday = (
      // |@| returns a cartesian builder – allows us to build tuples and to map them using an Applicative
      integer(input.birthYear) |@|
      integer(input.birthMonth) |@|
      integer(input.birthDay)
    ).tupled.andThen { case (year, month, day) => date(year, month, day)}

    (
      name(input.name) |@|
      email(input.email) |@|
      nonNegativeInteger(input.age) |@|
      birthday
    ).map(Person.apply)
  }

  val input = Input("Zdravko", "zstoychevgmail.com", "29", "f1988", "3", "31")

  validate(input) match {
    case Valid(person) => println(person)
    case Invalid(errors) =>
      println("The following issues have been found while parsing input:")
      errors.toList.foreach(println)
  }

  /*
  Result:

  The following issues have been found while parsing input:
  zstoychevgmail.com is not a valid email
  f1988 is not an integer
   */
}