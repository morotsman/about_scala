package scalaz_experiments.validation

import scalaz.Scalaz._
import scalaz._

import Validator._
import ValidatorMonad._

object ValidateMonad {

  case class Person(name: String, age: Int)

  def validateName(name: String): Validated[String] =
    if (name.length > 0 && name.length <= 21) Valid(name)
    else Invalid("Name validation failed")

  def validateAge(age: Int): Validated[Int] =
    if (age >= 0) Valid(age)
    else Invalid("Age failed the validation")

  def main(args: Array[String]): Unit = {
    val validator = Monad[Validated]

    val person1 = validator.apply2(validateName("NiklasNiklasNiklasNiklasNiklasNiklas"), validateAge(-1)) { (name, age) =>
      Person(name, age)
    }

    println(person1)

    val person2: Validated[Person] = ^(validateName("NiklasNiklasNiklasNiklasNiklasNiklas"), validateAge(-1)) { (name, age) =>
      Person(name, age)
    }
    assert(person2 == Invalid(Vector("Name validation failed")))
    println(person2)


    def createPerson(name: String, age: Int): Person =
      Person(name, age)

    val validPerson = Person("Niklas", 30)

    val person3: Validated[Person] = validator.lift2(createPerson)(
      validateName("NiklasNiklasNiklasNiklasNiklasNiklas"),
      validateAge(7)
    )

    println(person3)

    val person4 = (validateName("Niklas") |@| validateAge(30)) { (name: String, age: Int) => Person(name, age) }

    println(person4)

    val person5 = for {
      name <- validateName("Niklas")
      age <- validateAge(35)
    } yield (Person(name, age))

    assert(person5 == Valid(Person("Niklas",35)))
    println(person5)

    val person6 = for {
      name <- validateName("NiklasNiklasNiklasNiklasNiklasNiklasNiklas")
      age <- validateAge(-2)
    } yield (Person(name, age))

    assert(person6 == Invalid(Vector("Name validation failed")))

    println(person6)

    val person7 = validateName("Niklas").flatMap(name => validateAge(30).map(age => Person(name, age)))

    println(person7)
  }

}
