package scalaz

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import scalaz.Validate._
import Scalaz._

object ValidateSpecification extends Properties("Validate") {

  case class Person(name: String = "", books: Set[String] = Set(), salary: Int = 0)

  property("maxLengthString") = forAll { (name: String) =>
    val maxLength = 5
    val field = "name"
    val validator: Validator[Person] = maxLengthString[Person](field, maxLength)
    val person = Person(name)

    val result = validator(person)

    if (name.length < maxLength) {
      result == Right(person)
    } else {
      result == Left(ValidationException(s"Length on $field was ${name.length} but it must be less then $maxLength on $person"))
    }
  }

  property("minLengthString") = forAll { (name: String) =>
    val minLength = 2
    val field = "name"
    val validate = minLengthString[Person](field, minLength)
    val person = Person(name)

    val result = validate(person)

    if (name.length > minLength) {
      result == Right(person)
    } else {
      result == Left(ValidationException(s"Length on $field was ${name.length} but it must be greater then $minLength on $person"))
    }
  }

  property("compose min and max") = forAll { (name: String) =>
    val minLength = 2
    val maxLength = 5
    val field = "name"
    val minValidator = minLengthString[Person](field, minLength)
    val maxValidator = maxLengthString[Person](field, maxLength)
    val validate = Validate(minValidator) >==> maxValidator
    val person = Person(name)

    val result = validate(person)

    if (name.length <= minLength) {
      result == Left(ValidationException(s"Length on $field was ${name.length} but it must be greater then $minLength on $person"))
    } else if (name.length >= maxLength) {
      result == Left(ValidationException(s"Length on $field was ${name.length} but it must be less then $maxLength on $person"))
    } else {
      result == Right(person)
    }
  }

  property("minLength") = forAll { (books: Set[String]) =>
    val minSetLength = 2
    val field = "books"
    val validate = minLength[Set[_]][Person](field, minSetLength)
    val person = Person(books = books)

    val result = validate(person)

    if (books.length > minSetLength) {
      result == Right(person)
    } else {
      result == Left(ValidationException(s"Length on $field was ${books.size} but it must be greater then $minSetLength on $person"))
    }
  }

  property("maxLength") = forAll { (books: Set[String]) =>
    val maxSetLength = 5
    val field = "books"
    val validate = maxLength[Set[_]][Person](field, maxSetLength)
    val person = Person(books = books)

    val result = validate(person)

    if (books.length < maxSetLength) {
      result == Right(person)
    } else {
      result == Left(ValidationException(s"Length on $field was ${books.size} but it must be less then $maxSetLength on $person"))
    }
  }

  property("maxValue") = forAll { (salary: Int) =>
    val maxSalary = 3
    val field = "salary"
    val validate = maxValue[Person](field, maxSalary)
    val person = Person(salary = salary)

    val result = validate(person)

    if (salary < maxSalary) {
      result == Right(person)
    } else {
      result == Left(ValidationException(s"Value on $field was $salary but it must be less then $maxSalary on $person"))
    }
  }

  property("minValue") = forAll { (salary: Int) =>
    val minSalary = 3
    val field = "salary"
    val validate = minValue[Person](field, minSalary)
    val person = Person(salary = salary)

    val result = validate(person)

    if (salary > minSalary) {
      result == Right(person)
    } else {
      result == Left(ValidationException(s"Value on $field was $salary but it must be greater then $minSalary on $person"))
    }
  }

  property("wrong field name") = forAll { (name: String) =>
    val minSalary = 3

    try {
      val validator = minValue[Person](name, minSalary)
      false
    } catch {
      case e: RuntimeException => true
      case _ => false
    }
  }

  property("wrong type on field") = forAll { (salary: Int) =>
    val minSalary = 3
    val field = "name"
    val validate = minValue[Person](field, minSalary)

    val person = Person(salary = salary)

    val result = validate(person)
    result == Left(ValidationException(s"Wrong type on field $field:  ${new ClassCastException("Cannot cast java.lang.String to java.lang.Integer")}"))
  }

}
