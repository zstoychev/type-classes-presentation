package json

sealed trait JsonValue
case class JsonNumber(value: BigDecimal) extends JsonValue
case class JsonString(value: String) extends JsonValue
case class JsonBoolean(value: Boolean) extends JsonValue
case class JsonArray(value: Seq[JsonValue]) extends JsonValue
case class JsonObject(value: Map[String, JsonValue]) extends JsonValue
case object JsonNull extends JsonValue

object JsonValue {
  def toString(json: JsonValue): String = json match {
    case JsonNumber(value) => value.toString
    case JsonString(value) => s"""\"$value\"""
    case JsonBoolean(value) => value.toString
    case JsonArray(elements) => "[" + elements.map(toString).mkString(", ") + "]"
    case JsonObject(members) =>
      val membersStrings = members.map { case (key, value) =>
        s"""\"$key\": ${toString(value)}"""
      }
      "{" + membersStrings.mkString(", ") + "}"
    case JsonNull => "null"
  }
}

trait JsonSerializable[T] {
  def toJson(a: T): JsonValue
}

object JsonSerializable {
  def asJson[A](a: A)(implicit js: JsonSerializable[A]): JsonValue = js.toJson(a)

  implicit val intSerializable = new JsonSerializable[Int] {
    override def toJson(a: Int): JsonValue = JsonNumber(a)
  }

  implicit def listSerializable[A](implicit aSer: JsonSerializable[A]) =
    new JsonSerializable[List[A]] {
      override def toJson(a: List[A]): JsonValue =
        JsonArray(a.map(person => aSer.toJson(person)))
    }
}

case class Person(name: String, age: Int, email: String)

object Person {
  implicit val personSerializable = new JsonSerializable[Person] {
    override def toJson(a: Person): JsonValue = JsonObject(Map(
      "name" -> JsonString(a.name),
      "age" -> JsonNumber(a.age),
      "email" -> JsonString(a.email)
    ))
  }
}
