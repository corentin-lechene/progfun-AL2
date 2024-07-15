package example

import upickle.default.ReadWriter

final case class PetOwner(name: String, age: Int, pets: List[String])
    derives ReadWriter

@main
def jsonExample(): Unit = {
  val jsonString =
    """{"name": "Peter", "age": 13, "pets": ["Toolkitty", "Scaniel"]}"""

  // Lecture du json
  val json: ujson.Value = ujson.read(jsonString)

  // Extraction des valeurs dans le json
  val name: String = json("name").str
  val age: Int = json("age").num.toInt
  val pets: ujson.Value = json("pets")
  val firstPet: String = pets(0).str

  // On affiche les valeurs extraites
  println(s"name: $name, firstPet: $firstPet, age: $age")

  // On peut déserialiser le json vers une case class
  val owner = upickle.default.read[PetOwner](jsonString)
  println(
    s"${owner.name} is ${owner.age} old, the first pet name is ${owner.pets.head}"
  )

  // De manière analogue, on peut sérialiser notre objet en json
  val newOwner = owner.copy(name = "Jane", age = 18)
  val output: String = upickle.default.write(newOwner)
  println(s"new owner: ${output}")
}
