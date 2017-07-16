package json

object JsonTest extends App {
  implicit val personSerializable = new JsonSerializable[Person] {
    override def toJson(a: Person): JsonValue = JsonObject(Map(
      "name" -> JsonString(a.name),
      "age" -> JsonNumber(a.age)
    ))
  }


  println(JsonSerializable.asJson(
    List(
      Person("Ivan", 23, "aa@gmail.com"),
      Person("Ivan", 23, "aa@gmail.com")
    )
  ))
}
