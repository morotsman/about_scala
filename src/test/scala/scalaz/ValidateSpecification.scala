package scalaz

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import scalaz.Validate._

import scalaz._
import Scalaz._

object ValidateSpecification extends Properties("String") {

  case class Person(name: String)

  property("maxLengthString") = forAll { (name: String) =>
    val maxLength = 5
    val field = "name"
    val validator: Validator[Person] = maxLengthString[Person](field, maxLength)
    val person = Person(name)

    val result = validator(person)

    if (name.length < maxLength) {
      result == Right(Person(name))
    } else {
      result == Left(ValidationException(s"Length on $field was ${name.length} but it must be less then $maxLength on $person"))
    }
  }

  property("minLengthString") = forAll { (name: String) =>
    val minLength = 2
    val field = "name"
    val validator: Validator[Person] = minLengthString[Person](field, minLength)
    val person = Person(name)

    val result = validator(person)

    if (name.length > minLength) {
      result == Right(Person(name))
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
    val validator = Validate(minValidator) >==> maxValidator
    val person = Person(name)

    val result = validator(person)

    if (name.length <= minLength) {
      result == Left(ValidationException(s"Length on $field was ${name.length} but it must be greater then $minLength on $person"))
    } else if (name.length >= maxLength) {
      result == Left(ValidationException(s"Length on $field was ${name.length} but it must be less then $maxLength on $person"))
    } else {
      result == Right(Person(name))
    }
  }

}
