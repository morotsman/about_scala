package scalaz_experiments.kleisli_usage

import scalaz._
import Scalaz._

import scala.util.Either.RightProjection

object Usage {

  def main(args: Array[String]): Unit = {
    basicUsage()

    kleisliUsage()
  }

  private def basicUsage(): Unit = {
    val books = Set("Programing in Scala")
    val validPerson = Person("Niklas", "Malmö", books, 50000)
    val invalidPerson = Person("NiklasNiklasNiklasNiklasNiklasNiklasNiklas", "Malmö", books, 500000)

    def personValidatorWithFlatMap(p: Person): Either[String, Person] =
      nameValidator(p).flatMap(salaryValidator).flatMap(cityValidator)

    assert(Right(validPerson) == personValidatorWithFlatMap(validPerson))
    assert(Left("Name failed the validation") == personValidatorWithFlatMap(invalidPerson))

    def personValidatorWithFlatMap2(p: Person): Either[String, Person] =
      for {
        _ <- nameValidator(p)
        _ <- salaryValidator(p)
        _ <- cityValidator(p)
      } yield (p)

    assert(Right(validPerson) == personValidatorWithFlatMap2(validPerson))
    assert(Left("Name failed the validation") == personValidatorWithFlatMap2(invalidPerson))

    def addOne(i: Int): Int = i + 1

    def double(i: Int): Int = i * 2

    def addOneAndDouble(i: Int): Int = double(addOne(20))

    assert(42 == addOneAndDouble(20))

    val composedFunction: Int => Int = addOne _ andThen double
    assert(42 == composedFunction(20))

    def toAs(number: Int): String = Array.fill(number)("a").mkString

    val composedFunction2: Int => String = addOne _ andThen toAs
    assert("aaaaaa" == composedFunction2(5))

    // val composedFunction3 = (nameValidator _).andThen(salaryValidator) // will not compile, the types don't line up

    // andThen
    println("Kleisli")
    val validator = Kleisli(nameValidator) andThen Kleisli(salaryValidator) andThen Kleisli(cityValidator)
    println("Validator created")
    assert(Right(validPerson) == validator(validPerson))
    println("Validator has been executed")
    assert(Left("Name failed the validation") == validator(invalidPerson))

    val validator2 = Kleisli(nameValidator) >=> Kleisli(salaryValidator) >=> Kleisli(cityValidator)
    assert(Right(validPerson) == validator2(validPerson))
    assert(Left("Name failed the validation") == validator2(invalidPerson))

    val validator3 = Kleisli(nameValidator) >==> salaryValidator >==> cityValidator
    assert(Right(validPerson) == validator3(validPerson))
    assert(Left("Name failed the validation") == validator3(invalidPerson))

    val v: Kleisli[({type f[x] = Either[String, x]})#f, Person, Person] = Kleisli(nameValidator)
    val result: Either[String, Person] = v(validPerson)


    def personToPerson(p: Person): Person = p
    // Kleisli(personToPerson) // will not compile

    def personToPersons(p: Person): List[Person] = List(p)

    Kleisli(personToPersons)

    val b = implicitly[Bind[({type f[x] = Either[String, x]})#f]]

    assert(Right(2) == b.bind(Right(1))(i => Right(i + 1)))
    assert(Left("failure") == b.bind(Left("failure"): Either[String, Int])(i => Right(i + 1)))

    /*
    def >=>[C](k: Kleisli[M, B, C])(implicit b: Bind[M]): Kleisli[M, A, C] =  kleisli((a: A) => b.bind(this(a))(k.run))
     */
    Kleisli(nameValidator) >=> Kleisli(salaryValidator)

    val substitutedValidator = Kleisli((a: Person) => b.bind(Kleisli(nameValidator)(a))(Kleisli(salaryValidator).run))

    assert(Right(validPerson) == substitutedValidator(validPerson))
    assert(Left("Name failed the validation") == substitutedValidator(invalidPerson))

    val substitutedValidator2 = Kleisli((a: Person) => b.bind(nameValidator(a))(salaryValidator))

    assert(Right(validPerson) == substitutedValidator2(validPerson))
    assert(Left("Name failed the validation") == substitutedValidator2(invalidPerson))

    assert(Right(validPerson) == b.bind(nameValidator(validPerson))(salaryValidator))

  }

  case class Person(name: String, city: String, books: Set[String], salary: Int)

  def nameValidator(p: Person): Either[String, Person] = {
    // println("nameValidator")
    if (p.name.length > 0 && p.name.length < 21) Right(p) else Left("Name failed the validation")
  }

  def cityValidator(p: Person): Either[String, Person] = {
    // println("cityValidator")
    if (p.city.length > 0 && p.city.length < 21) Right(p) else Left("City failed the validation")
  }

  def salaryValidator(p: Person): Either[String, Person] = {
    // println("salaryValidator")
    if (p.salary > 0 && p.salary < 100000) Right(p) else Left("Salary failed the validation")
  }

  private def kleisliUsage(): Unit = {
    val books = Set("Programing in Scala")
    val validPerson = Person("Niklas", "Malmö", books, 150000)
    val invalidPerson = Person("NiklasNiklasNiklasNiklasNiklasNiklasNiklas", "Malmö", books, 500000)

    val personValidator = Kleisli(nameValidator) >==> cityValidator
    assert(Right(validPerson) == personValidator(validPerson))

    // traverse
    assert(
      Right(List(validPerson, validPerson)) == personValidator.traverse(List(validPerson, validPerson))
    )
    assert(Left("Name failed the validation") == personValidator.traverse(List(validPerson, validPerson, invalidPerson)))

    // map
    assert(Right(true) == (personValidator.map(_ => true)).run(validPerson))

    // mapT && mapK
    val optionValidator = personValidator.mapT((ep: Either[String, Person]) => if (ep.isRight) Option(ep.right) else None)
    assert(Some(RightProjection(Right(validPerson))) == optionValidator(validPerson))

    // flatMapK
    val validator = personValidator.flatMapK((p: Person) => Right(p))
    assert((validator(validPerson) == Right(validPerson)))
    // personValidator3.flatMapK(p => Option(p)) // will not compile

    // flatMap
    val withSalary = personValidator.flatMap((p: Person) => Kleisli(salaryValidator))
    assert(Left("Salary failed the validation") == withSalary(validPerson))

    // lift
    val result = personValidator.lift(Applicative[List])
    assert(List(Right(validPerson)) == result(validPerson))

    // transform


  }

}
