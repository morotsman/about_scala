package scalaz_experiments.validation

import scalaz._
import Scalaz._
import Validator._
import ValidatorApplicative._

object ValidateApplicative {

  case class Person(name: String, age: Int)

  def validateName(name: String): Validated[String] =
    if (name.length > 0 && name.length <= 21) Valid(name)
    else Invalid("Name validation failed")

  def validateAge(age: Int): Validated[Int] =
    if (age >= 0) Valid(age)
    else Invalid("Age failed the validation")

  def main(args: Array[String]): Unit = {
    basicExamples()
    validatorExample()
  }

  def basicExamples(): Unit = {
    val someOne = 1.some
    val someTwo = 2.some
    val someThree = 3.some

    def addOne(a: Int) = a + 1

    val result1 = someOne.map(addOne)
    assert(result1 == Some(2))

    val result2 = someTwo <*> (addOne _).some
    assert(result2 == Some(3))

    def addTwo(a: Int, b: Int): Int = a + b


    val result6 = someOne.map(a => someTwo.map(b => addTwo(a, b)))
    assert(result6 == Some(Some(3)))

    // if we're using Option as a monad we have access to flatMap...
    val result5 = for {
      a <- someOne
      b <- someTwo
    } yield addTwo(a, b)
    assert(result5 == Some(3))

    val result7 = ^(someOne, someTwo)(addTwo)
    assert(result7 == Some(3))

    val result8 = (someOne |@| someTwo) (addTwo)
    assert(result8 == Some(3))

    def addFour(a: Int, b: Int, c: Int, d: Int): Int = a + b + c + d

    val result9 = ^^^(someOne, someTwo, someOne, someTwo)(addFour)
    assert(result9 == Some(6))

    val result10 = (someOne |@| someTwo |@| someOne |@| someTwo) (addFour)
    assert(result10 == Some(6))

    val result11 = someTwo +++ someTwo +++ someTwo
    assert(result11 == Some(6))

    implicit val times: Semigroup[Int] = new Semigroup[Int] {
      override def append(f1: Int, f2: => Int): Int = f1 * f2
    }

    val result12 = Applicative[Option].plusA(someThree, someThree)(times)
    assert(result12 == Some(9))

    val result13 = someThree.+++(someTwo)(times) +++ someTwo
    assert(result13 == Some(8))
  }

  def validatorExample(): Unit = {
    val validator = Applicative[Validated]

    val person1 = validator.apply2(validateName("NiklasNiklasNiklasNiklasNiklasNiklas"), validateAge(-1)) { (name, age) =>
      Person(name, age)
    }

    assert(person1 == Invalid(Vector("Age failed the validation", "Name validation failed")))

    val person2: Validated[Person] = ^(validateName("NiklasNiklasNiklasNiklasNiklasNiklas"), validateAge(-1)) { (name, age) =>
      Person(name, age)
    }

    assert(person2 == Invalid(Vector("Age failed the validation", "Name validation failed")))

    def createPerson(name: String, age: Int): Person =
      Person(name, age)

    val person3: Validated[Person] = validator.lift2(createPerson)(
      validateName("NiklasNiklasNiklasNiklasNiklasNiklas"),
      validateAge(7)
    )
    assert(person3 == Invalid(Vector("Name validation failed")))

    val result = (validateName("Niklas") |@| validateAge(30)) { (name: String, age: Int) => Person(name, age) }
    assert(result == Valid(Person("Niklas", 30)))
  }

}
