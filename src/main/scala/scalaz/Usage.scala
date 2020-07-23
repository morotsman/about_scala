package scalaz

import scalaz._
import Scalaz._

import Validate._


object Usage {

  case class Person(name: String, age: Int, city: String, books: Set[String])

  def nameValidator(p: Person): Either[String, Person] =
    if (p.name.length > 0 && p.name.length < 21) Right(p) else Left("Name failed the validation")

  def ageValidator(p: Person): Either[String, Person] =
    if (p.age > 0 && p.age < 121) Right(p) else Left("Age failed the validation")

  def cityValidator(p: Person): Either[String, Person] =
    if (p.city.length > 0 && p.city.length < 21) Right(p) else Left("City failed the validation")


  def addOne(i: Int): Int = i + 1

  def double(i: Int): Int = i * 2

  def as(number: Int): String = Array.fill(number)("a").mkString

  def main(args: Array[String]): Unit = {
    val books = Set("hepp")
    val validPerson = Person("Niklas", 47, "Malmö", books)
    val invalidPerson = Person("NiklasNiklasNiklasNiklasNiklasNiklasNiklas", 47, "Malmö", books)

    def personValidator1(p: Person): Either[String, Person] =
      nameValidator(p).flatMap(ageValidator).flatMap(cityValidator)

    println(personValidator1(validPerson))

    println("*************************")

    val composedFunction1: Int => Int = addOne _ andThen double
    println(composedFunction1(20))

    val composedFunction2: Int => String = addOne _ andThen as
    println(composedFunction2(5))

    // val composedFunction3 = (nameValidator _).andThen(ageValidator) // will not compile, the types don't line up

    // andThen
    val personValidator2 = Kleisli(nameValidator) >=> Kleisli(ageValidator) >=> Kleisli(cityValidator)
    println(personValidator2(validPerson))

    val personValidator3 = Kleisli(nameValidator) >==> ageValidator >==> cityValidator
    println(personValidator3(validPerson))

    // traverse
    println(personValidator3.traverse(List(validPerson, validPerson)))
    println(personValidator3.traverse(List(validPerson, validPerson, invalidPerson)))

    // map
    println((personValidator3.map(_ => true)).run(validPerson))

    println("********************************")

    val personValidator100 =
      Kleisli(maxLength[Person]("name", 21)) >==>
        minLengthString("name", 0) >==>
        maxValue("age", 121) >==>
        minValue("age", 0) >==>
        maxLength("city", 21) >==>
        minLengthString("city", 0) >==>
        minLength[Person, Set[String]]("books", 0)

    println(personValidator100(Person("NiklasNiklasNiklasNiklas", 46, "Malmö", books)))
    println(personValidator100(Person("", 120, "Malmö", books)))
    println(personValidator100(Person("Adam", 121, "Malmö", books)))
    println(personValidator100(Person("Adam", 120, "Malmöööööööööööööööööööööööööööööö", books)))
    println(personValidator100(validPerson))
  }

}
