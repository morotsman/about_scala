package scalaz_experiments.validation

import scalaz.Scalaz._
import scalaz._

object ValidateMonad {

  sealed trait Validator[+E, +T]

  case class Valid[+T](t: T) extends Validator[Nothing, T]

  case class Invalid[+E](es: Vector[E]) extends Validator[E, Nothing]

  object Invalid {
    def apply[E](e: E): Invalid[E] = Invalid(Vector(e))
  }

  type Validated[T] = Validator[String, T]

  implicit val validationMonad: Monad[Validated] = new Monad[Validated] {
    override def point[A](a: => A): Validated[A] = Valid(a)

    override def bind[A, B](fa: Validated[A])(f: A => Validated[B]): Validated[B] = fa match {
      case Valid(a) => f(a)
      case Invalid(i) => Invalid(i)
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
    val validator = Monad[Validated]

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
