package scalaz_experiments.validation

import scalaz._
import Scalaz._

import Validator._

object ValidateApplicative {

  implicit val validationApplicative: Applicative[Validated] = new Applicative[Validated] {
    override def point[A](a: => A): Validated[A] = Valid(a)

    override def ap[A, B](fa: => Validated[A])(f: => Validated[A => B]): Validated[B] = (fa, f) match {
      case (Valid(a), Valid(fab)) => Valid(fab(a))
      case (Valid(a), Invalid(es)) => Invalid(es)
      case (Invalid(es), Valid(a)) => Invalid(es)
      case (Invalid(es1), Invalid(es2)) => Invalid(es1 ++ es2)
    }
  }

  case class Person(name: String, age: Int)

  def validateName(name: String): Validated[String] =
    if (name.length > 0 && name.length <= 21) Valid(name)
    else Invalid("Name validation failed")

  def validateAge(age: Int): Validated[Int] =
    if (age >= 0) Valid(age)
    else Invalid("Age failed the validation")

  def main(args: Array[String]): Unit = {
    val validator = Applicative[Validated]

    val person1 = validator.apply2(validateName("NiklasNiklasNiklasNiklasNiklasNiklas"), validateAge(-1)) { (name, age) =>
      Person(name, age)
    }

    println(person1)

    val person2: Validated[Person] = ^(validateName("NiklasNiklasNiklasNiklasNiklasNiklas"), validateAge(-1)) { (name, age) =>
      Person(name, age)
    }

    println(person2)


    def createPerson(name: String, age: Int): Person =
      Person(name, age)

    val validPerson = Person("Niklas", 30)

    val person3: Validated[Person] = validator.lift2(createPerson)(
      validateName("NiklasNiklasNiklasNiklasNiklasNiklas"),
      validateAge(7)
    )

    println(person3)

    val result = (validateName("Niklas") |@| validateAge(30)) { (name: String, age: Int) => Person(name, age)}

    println(result)



  }

}
