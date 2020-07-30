package scalaz_experiments.validation

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

  val max20 = maxLengthString(20)
  val between = (minLengthString(0) && maxLengthString(20)) (((t, v, f) => s"Value on $f was $v but it must be between 0 and 20 for $t"))

  private val personValidator =
    Validate[Person] >=>
      (maxLengthString(20) on "name") >=>
      (minLengthString(0) on "name") >=>
      (maxLengthString(20) on "city") >=>
      (minLengthString(0) on "city") >=>
      (minLength[Set[_]](0) on "books") >=>
      (maxLength[Set[_]](1000) on "books") >=>
      (minValue(0) on "salary") >=>
      (maxValue(100000) on "salary") >=>
      (setContains("Programing in Scala") on "books")

  def setContains[T](title: String): Validator[Set[String]] =
    Validator[Set[String]](
      s => s.contains(title),
      (t, v, f) => s"The set did not contain $title: $t"
    )

  private def validatorExample(): Unit = {
    val max21: Validator[String] = maxLengthString[Person](20)
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
    assert(s"Left(scalaz_experiments.validation.ValidationException: Length on name was 42 but it must be less or equal to 20 for $invalidPerson)" == result2.toString)

    val result3: Either[Exception, Int] = avgSalaryWithLogging(List())
    assert("Left(scalaz_experiments.validation.Usage$AvgException$1: Avg on empty list)" == result3.toString)

  }

}
