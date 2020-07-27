package scalaz

import scalaz._
import Scalaz._
import Validate._

import scala.util.Either.RightProjection


object Usage {

  def main(args: Array[String]): Unit = {
    basicUsage()

    kleisliUsage()

    validatorExample()
  }

  private def basicUsage() = {
    val books = Set("Programing in Scala")
    val validPerson = Person("Niklas", 47, "Malmö", books, 50000)
    val invalidPerson = Person("NiklasNiklasNiklasNiklasNiklasNiklasNiklas", 47, "Malmö", books, 500000)

    def personValidator1(p: Person): Either[String, Person] =
      nameValidator(p).flatMap(ageValidator).flatMap(cityValidator)

    assert(Right(Person("Niklas", 47, "Malmö", Set("Programing in Scala"), 50000)) == personValidator1(validPerson))
    assert(Left("Name failed the validation") == personValidator1(invalidPerson))

    def addOne(i: Int): Int = i + 1

    def double(i: Int): Int = i * 2

    val composedFunction1: Int => Int = addOne _ andThen double
    assert(42 == composedFunction1(20))

    def toAs(number: Int): String = Array.fill(number)("a").mkString

    val composedFunction2: Int => String = addOne _ andThen toAs
    assert("aaaaaa" == composedFunction2(5))

    // val composedFunction3 = (nameValidator _).andThen(ageValidator) // will not compile, the types don't line up

    // andThen
    val personValidator2 = Kleisli(nameValidator) >=> Kleisli(ageValidator) >=> Kleisli(cityValidator)
    assert(Right(Person("Niklas", 47, "Malmö", Set("Programing in Scala"), 50000)) == personValidator2(validPerson))
    assert(Left("Name failed the validation") == personValidator2(invalidPerson))
  }

  case class Person(name: String, age: Int, city: String, books: Set[String], salary: Int)

  def nameValidator(p: Person): Either[String, Person] =
    if (p.name.length > 0 && p.name.length < 21) Right(p) else Left("Name failed the validation")

  def ageValidator(p: Person): Either[String, Person] =
    if (p.age > 0 && p.age < 121) Right(p) else Left("Age failed the validation")

  def cityValidator(p: Person): Either[String, Person] =
    if (p.city.length > 0 && p.city.length < 21) Right(p) else Left("City failed the validation")

  def salaryValidator(p: Person): Either[String, Person] =
    if (p.salary > 0 && p.salary < 20000) Right(p) else Left("Salary failed the validation")

  private def kleisliUsage() = {
    val books = Set("Programing in Scala")
    val validPerson = Person("Niklas", 47, "Malmö", books, 50000)
    val invalidPerson = Person("NiklasNiklasNiklasNiklasNiklasNiklasNiklas", 47, "Malmö", books, 500000)

    val personValidator = Kleisli(nameValidator) >==> ageValidator >==> cityValidator
    assert(Right(Person("Niklas", 47, "Malmö", Set("Programing in Scala"), 50000)) == personValidator(validPerson))

    // traverse
    assert(
      Right(List(
        Person("Niklas", 47, "Malmö", Set("Programing in Scala"), 50000),
        Person("Niklas", 47, "Malmö", Set("Programing in Scala"), 50000)
      )) == personValidator.traverse(List(validPerson, validPerson))
    )
    assert(Left("Name failed the validation") == personValidator.traverse(List(validPerson, validPerson, invalidPerson)))

    // map
    assert(Right(true) == (personValidator.map(_ => true)).run(validPerson))

    // mapT && mapK
    val optionValidator = personValidator.mapT((ep: Either[String, Person]) => if (ep.isRight) Option(ep.right) else None)
    assert(Some(RightProjection(Right(Person("Niklas", 47, "Malmö", Set("Programing in Scala"), 50000)))) == optionValidator(validPerson))

    // flatMapK
    val validator = personValidator.flatMapK((p: Person) => Right(p))
    assert((validator(validPerson) == Right(Person("Niklas", 47, "Malmö", Set("Programing in Scala"), 50000))))
    // personValidator3.flatMapK(p => Option(p)) // will not compile

    // flatMap
    val withSalary = personValidator.flatMap((p: Person) => Kleisli(salaryValidator))
    assert(Left("Salary failed the validation") == withSalary(validPerson))

    // lift
    val result = personValidator.lift(Applicative[List])
    assert(List(Right(Person("Niklas", 47, "Malmö", Set("Programing in Scala"), 50000))) == result(validPerson))

    // transform


  }

  private def validatorExample() = {
    println("**************************")

    val personValidator =
      Kleisli(maxLength[Person]("name", 21)) >==>
        minLengthString("name", 0) >==>
        maxValue("age", 121) >==>
        minValue("age", 0) >==>
        maxLength("city", 21) >==>
        minLengthString("city", 0) >==>
        minLength[Set[String]]("books", 0) >==>
        minValue("salary", 0)

    def logInput(lp: List[Person]): Either[Exception, List[Person]] = {
      println(s"Will calculate the avg salary for ${lp.size} persons")
      Right(lp)
    }

    def validatePersons(lp: List[Person]): Either[Exception, List[Person]] =
      personValidator traverse lp

    final case class AvgException(message: String) extends Exception(message)

    def calculateAvgSalary(lp: List[Person]): Either[Exception, Int] =
      if (lp.isEmpty) {
        Left(AvgException("Avg on empty list"))
      } else {
        val (tot, sum) = lp.map(_.salary).foldLeft((0, 0))((acc, s) => (acc._1 + 1, acc._2 + s))
        Right(sum / tot)
      }

    def logSuccess(salary: Int): Either[Exception, Int] = {
      println(s"The avarage salery is: $salary")
      Right(salary)
    }

    val avgSalary = Kleisli(validatePersons) >==> calculateAvgSalary

    val avgSalaryWithLogging = Kleisli(logInput) >==> avgSalary >==> logSuccess

    val books = Set("Programing in Scala")
    val validPerson = Person("Niklas", 47, "Malmö", books, 50000)
    val validPerson2 = Person("Niklas", 47, "Malmö", books, 80000)
    val invalidPerson = Person("NiklasNiklasNiklasNiklasNiklasNiklasNiklas", 47, "Malmö", books, 500000)

    val result1: Either[Exception, Int] = avgSalaryWithLogging(List(validPerson, validPerson2))
    println(result1)
    assert(Right(65000) == result1)

    val result2: Either[Exception, Int] = avgSalaryWithLogging(List(validPerson, validPerson2, invalidPerson))
    assert("Left(scalaz.ValidationException: Length on name was 42 but it must be less then 21 on Person(NiklasNiklasNiklasNiklasNiklasNiklasNiklas,47,Malmö,Set(Programing in Scala),500000))" == result2.toString)

    val result3: Either[Exception, Int] = avgSalaryWithLogging(List())
    assert("Left(scalaz.Usage$AvgException$1: Avg on empty list)" == result3.toString)
  }

}
