package scalaz_experiments.validation

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import Validate._

import scalaz._
import Scalaz._

object ValidateSpecification extends Properties("Validate") {

  case class Person(name: String = "", books: Set[String] = Set(), salary: Int = 0)

  property("maxLengthString") = forAll { (name: String) =>
    val maxLength = 5
    val field = "name"
    val validator: FieldValidator[Person] = maxLengthString(maxLength).on[Person](field)
    val person = Person(name)

    val result = validator(person)

    if (name.length <= maxLength) {
      result == Right(person)
    } else {
      result == Left(ValidationException(s"Length on $field was ${name.length} but it must be less or equal to $maxLength for $person"))
    }
  }

  property("minLengthString") = forAll { (name: String) =>
    val minLength = 2
    val field = "name"
    val validate = minLengthString(minLength).on[Person](field)
    val person = Person(name)

    val result = validate(person)

    if (name.length >= minLength) {
      result == Right(person)
    } else {
      result == Left(ValidationException(s"Length on $field was ${name.length} but it must be greater or equal to $minLength for $person"))
    }
  }

  property("compose min and max") = forAll { (name: String) =>
    val minLength = 2
    val maxLength = 5
    val field = "name"
    val minValidator = minLengthString(minLength).on[Person](field)
    val maxValidator = maxLengthString[Person](maxLength).on[Person](field)
    val validate = Validate[Person] >=> minValidator >=> maxValidator
    val person = Person(name)

    val result = validate(person)

    if (name.length < minLength) {
      result == Left(ValidationException(s"Length on $field was ${name.length} but it must be greater or equal to $minLength for $person"))
    } else if (name.length > maxLength) {
      result == Left(ValidationException(s"Length on $field was ${name.length} but it must be less or equal to $maxLength for $person"))
    } else {
      result == Right(person)
    }
  }

  property("minLength") = forAll { (books: Set[String]) =>
    val minSetLength = 2
    val field = "books"
    val validate = minLength[Set[_]](minSetLength).on[Person](field)
    val person = Person(books = books)

    val result = validate(person)

    if (books.length >= minSetLength) {
      result == Right(person)
    } else {
      result == Left(ValidationException(s"Length on $field was ${books.size} but it must be greater or equal to $minSetLength for $person"))
    }
  }

  property("maxLength") = forAll { (books: Set[String]) =>
    val maxSetLength = 5
    val field = "books"
    val validate = maxLength[Set[_]](maxSetLength).on[Person](field)
    val person = Person(books = books)

    val result = validate(person)

    if (books.length <= maxSetLength) {
      result == Right(person)
    } else {
      result == Left(ValidationException(s"Length on $field was ${books.size} but it must be less or equal to $maxSetLength for $person"))
    }
  }

  property("maxValue") = forAll { (salary: Int) =>
    val maxSalary = 3
    val field = "salary"
    val validate = maxValue(maxSalary).on[Person](field)
    val person = Person(salary = salary)

    val result = validate(person)

    if (salary <= maxSalary) {
      result == Right(person)
    } else {
      result == Left(ValidationException(s"Value on $field was $salary but it must be less or equal to $maxSalary for $person"))
    }
  }

  property("minValue") = forAll { (salary: Int) =>
    val minSalary = 3
    val field = "salary"
    val validate = minValue(minSalary).on[Person](field)
    val person = Person(salary = salary)

    val result = validate(person)

    if (salary >= minSalary) {
      result == Right(person)
    } else {
      result == Left(ValidationException(s"Value on $field was $salary but it must be greater or equal to $minSalary for $person"))
    }
  }

  property("wrong field name") = forAll { (name: String) =>
    val minSalary = 3

    try {
      val validator = minValue(minSalary).on[Person](name)
      false
    } catch {
      case e: RuntimeException => true
      case _ => false
    }
  }

  property("wrong type on field") = forAll { (salary: Int) =>
    val minSalary = 3
    val field = "name"
    val validate = minValue(minSalary).on[Person](field)

    val person = Person(salary = salary)

    val result = validate(person)
    result == Left(ValidationException(s"Wrong type on field $field:  ${new ClassCastException("Cannot cast java.lang.String to java.lang.Integer")}"))
  }

}
