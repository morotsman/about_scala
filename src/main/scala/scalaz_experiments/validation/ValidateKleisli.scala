package scalaz_experiments.validation

import scalaz.Scalaz._
import scalaz._

import Validator._
import ValidatorMonad.validationMonad

object ValidateKleisli {

  case class Person(name: String, age: Int)

  def validateName(p: Person): Validated[Person] =
    if (p.name.length > 0 && p.name.length <= 21) Valid(p)
    else Invalid("Name validation failed")

  def validateAge(p: Person): Validated[Person] =
    if (p.age >= 0) Valid(p)
    else Invalid("Age failed the validation")


  def main(args: Array[String]): Unit = {
    val validator = Kleisli(validateName) >=> Kleisli(validateAge)

    val validPerson = Person("Niklas", 40)
    val invalidPerson = Person("NiklasNiklasNiklasNiklasNiklasNiklas", -2)

    println(validator(validPerson))
    println(validator(invalidPerson))
  }

}
