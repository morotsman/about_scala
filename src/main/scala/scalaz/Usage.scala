package scalaz

import scalaz._
import Scalaz._

import Validate._


object Usage {

  case class Person(name: String, age: Int, city: String, books: Set[String], salary: Int)

  def nameValidator(p: Person): Either[String, Person] =
    if (p.name.length > 0 && p.name.length < 21) Right(p) else Left("Name failed the validation")

  def ageValidator(p: Person): Either[String, Person] =
    if (p.age > 0 && p.age < 121) Right(p) else Left("Age failed the validation")

  def cityValidator(p: Person): Either[String, Person] =
    if (p.city.length > 0 && p.city.length < 21) Right(p) else Left("City failed the validation")


  def addOne(i: Int): Int = i + 1

  def double(i: Int): Int = i * 2

  def as(number: Int): String = Array.fill(number)("a").mkString

  private val personValidator100 =
    Kleisli(maxLength[Person]("name", 21)) >==>
      minLengthString("name", 0) >==>
      maxValue("age", 121) >==>
      minValue("age", 0) >==>
      maxLength("city", 21) >==>
      minLengthString("city", 0) >==>
      minLength[Set[String]]("books", 0) >==>
      minValue("salary", 0)

  def main(args: Array[String]): Unit = {
    val books = Set("hepp")
    val validPerson = Person("Niklas", 47, "Malmö", books, 50000)
    val validPerson2 = Person("Niklas", 47, "Malmö", books, 80000)
    val invalidPerson = Person("NiklasNiklasNiklasNiklasNiklasNiklasNiklas", 47, "Malmö", books, 500000)

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

    // mapT && mapK
    val optionValidator = personValidator3.mapT(e => if (e.isRight) Option(e.right) else None)
    println(optionValidator(validPerson))

    // flatMapK


    println("************Validator********************")
    println(personValidator100(Person("NiklasNiklasNiklasNiklas", 46, "Malmö", books, 10000)))
    println(personValidator100(Person("", 120, "Malmö", books, 20000)))
    println(personValidator100(Person("Adam", 121, "Malmö", books, 30000)))
    println(personValidator100(Person("Adam", 120, "Malmöööööööööööööööööööööööööööööö", books, 40000)))
    println(personValidator100(Person("Adam", 120, "Malmö", books, 0)))
    println(personValidator100(validPerson))

    def logInput(lp: List[Person]): Either[Exception, List[Person]] = {
      println(s"Will calculate the avg salary for ${lp.size} persons")
      Right(lp)
    }

    def validatePersons(lp: List[Person]) : Either[Exception, List[Person]] =
      personValidator100 traverse lp

    final case class AvgException(message: String) extends Exception(message)

    def calculateAvgSalary(lp: List[Person]): Either[Exception, Int] =
      if(lp.size == 0) {
        Left(AvgException("Avg on empty list"))
      } else {
        val (tot, sum) = lp.map(_.salary).foldLeft((0,0))((acc, s) => (acc._1 + 1, acc._2 + s))
        Right(sum/tot)
      }

    def logSuccess(salary: Int): Either[Exception, Int] = {
      println(s"The avarage salery is: $salary")
      Right(salary)
    }

    val avgSalary =
        Kleisli(validatePersons) >==>
        calculateAvgSalary

    val avgSalaryWithLogging =
      Kleisli(logInput) >==>
        avgSalary >==>
        logSuccess

    val result1: Either[Exception, Int] = avgSalaryWithLogging(List(validPerson, validPerson2))
    println(result1)

    val result2: Either[Exception, Int] = avgSalaryWithLogging(List(validPerson, validPerson2, invalidPerson))
    println(result2)

    val result3: Either[Exception, Int] = avgSalaryWithLogging(List())
    println(result3)

  }

}
