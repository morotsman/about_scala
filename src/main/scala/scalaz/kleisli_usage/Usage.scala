package scalaz.kleisli_usage

import scalaz.Scalaz._
import Validate._
import scalaz.{Applicative, Kleisli}

import scala.util.Either.RightProjection

object Usage {

  def main(args: Array[String]): Unit = {
    basicUsage()

    kleisliUsage()

    validatorExample()
  }

  private def basicUsage() = {
    val books = Set("Programing in Scala")
    val validPerson = Person("Niklas", "Malmö", books, 50000)
    val invalidPerson = Person("NiklasNiklasNiklasNiklasNiklasNiklasNiklas", "Malmö", books, 500000)

    def personValidator1(p: Person): Either[String, Person] =
      nameValidator(p).flatMap(salaryValidator).flatMap(cityValidator)

    assert(Right(validPerson) == personValidator1(validPerson))
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
    val personValidator2 = Kleisli(nameValidator) >=> Kleisli(salaryValidator) >=> Kleisli(cityValidator)
    assert(Right(validPerson) == personValidator2(validPerson))
    assert(Left("Name failed the validation") == personValidator2(invalidPerson))
  }

  case class Person(name: String, city: String, books: Set[String], salary: Int)

  def nameValidator(p: Person): Either[String, Person] =
    if (p.name.length > 0 && p.name.length < 21) Right(p) else Left("Name failed the validation")

  def cityValidator(p: Person): Either[String, Person] =
    if (p.city.length > 0 && p.city.length < 21) Right(p) else Left("City failed the validation")

  def salaryValidator(p: Person): Either[String, Person] =
    if (p.salary > 0 && p.salary < 100000) Right(p) else Left("Salary failed the validation")

  private def kleisliUsage() = {
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

  val personValidator =
    Validate[Person](maxLengthString(20) on "name") >==>
      (minLengthString(0) on "name") >==>
      (maxLengthString(20) on "city") >==>
      (minLengthString(0) on "city") >==>
      (minLength[Set[_]](0) on "books") >==>
      (maxLength[Set[_]](1000) on "books") >==>
      (minValue(0) on "salary") >==>
      (maxValue(100000) on "salary")

  private def validatorExample() = {
    val max21: Validator[Person, String] = maxLengthString[Person](20)
    val tmp: FieldValidator[Person] = max21.on("name")

    val tmp2: FieldValidator[Person] = maxLengthString[Person](20).on("name")

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
    val validPerson = Person("Niklas", "Malmö", books, 50000)
    val validPerson2 = Person("Niklas", "Malmö", books, 80000)
    val invalidPerson = Person("NiklasNiklasNiklasNiklasNiklasNiklasNiklas", "Malmö", books, 500000)

    val result1: Either[Exception, Int] = avgSalaryWithLogging(List(validPerson, validPerson2))
    println(result1)
    assert(Right(65000) == result1)

    val result2: Either[Exception, Int] = avgSalaryWithLogging(List(validPerson, validPerson2, invalidPerson))
    println(result2)
    assert(s"Left(scalaz.kleisli_usage.ValidationException: Length on name was 42 but it must be less or equal to 20 for $invalidPerson)" == result2.toString)

    val result3: Either[Exception, Int] = avgSalaryWithLogging(List())
    assert("Left(scalaz.kleisli_usage.Usage$AvgException$1: Avg on empty list)" == result3.toString)

  }

}
