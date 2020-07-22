package scalaz

import scalaz._
import Scalaz._

import scala.reflect.ClassTag

import scala.reflect.runtime.universe._
import scala.language.reflectiveCalls

object KleisliUsage {

  type Validator[T] = T => Either[String, T]

  case class Person(name: String, age: Int, city: String, books: List[String])

  // https://stackoverflow.com/questions/6686992/scala-asinstanceof-with-parameterized-types
  def cast[A <: Any : Manifest](a: Any): A =
    manifest[A].runtimeClass.cast(a).asInstanceOf[A]

  def getValue[I, O](obj: I, field: String)(implicit ct: ClassTag[I], tt: TypeTag[I], ct1: ClassTag[O], tt1: TypeTag[O]): Either[String, O] = {
    try {
      val symbol = typeOf[I].member(TermName(field)).asMethod
      val m = runtimeMirror(obj.getClass.getClassLoader)
      val im = m.reflect(obj)
      val value: O = cast[O](im.reflectMethod(symbol).apply())
      Right(value)
    } catch {
      case e: ClassCastException => Left(s"Wrong type on field $field:  $e")
      case e: ScalaReflectionException => Left(s"Could not find a field called $field: $e")
    }
  }

  private def maxLength[T](field: String, max: Int)(implicit ct: ClassTag[T], tt: TypeTag[T], m: Manifest[T]): Validator[T] = {
    validateField(field)
    (t: T) => {
      for {
        value <- getValue[T, String](t, field)
        result <- if (value.length < max) Right(t) else Left(s"Length on $field was ${value.length} but it must be less then $max")
      } yield result
    }
  }

  private def validateField[T](field: String)(implicit m: Manifest[T]) = {
    val fieldExists = manifest[T].runtimeClass.getMethods.exists(m => m.getName == field)
    if (!fieldExists) throw new RuntimeException(s"Could not find a field called $field")
  }

  private def getType[T](field: String)(implicit m: Manifest[T]) = {
    manifest[T].runtimeClass.getMethods.filter(m => m.getName == field).map(m => m.getAnnotatedReturnType).head
  }

  private def minLength[T](field: String, min: Int)(implicit ctf: ClassTag[T], ttf: TypeTag[T], m: Manifest[T]): Validator[T] = {
    validateField(field)
    (t: T) => {
      for {
        value <- getValue[T, String](t, field)
        result <- if (value.length > min) Right(t) else Left(s"Length on $field was ${value.length} but it most be greater then $min")
      } yield result
    }
  }

  private def maxValue[T](field: String, max: Int)(implicit ct: ClassTag[T], tt: TypeTag[T], m: Manifest[T]): Validator[T] = {
    validateField(field)
    (t: T) => {
      for {
        value <- getValue[T, Integer](t, field)
        result <- if (value < max) Right(t) else Left(s"Value on $field was $value but it must be less then $max")
      } yield result
    }
  }

  private def minValue[T](field: String, min: Int)(implicit ct: ClassTag[T], tt: TypeTag[T], m: Manifest[T]): Validator[T] = {
    validateField(field)
    (t: T) => {
      for {
        value <- getValue[T, Integer](t, field)
        result <- if (value > min) Right(t) else Left(s"Value on $field was $value but it must be greater then $min")
      } yield result
    }
  }

  implicit def validatorToKleisli[T](v: Validator[T]) =
    Kleisli(v)

  def main(args: Array[String]): Unit = {

    val books = List("hepp")

    val tmp1 = getValue[Person, String](Person("Beata", 6, "Malmö", books), "name2")
    println(tmp1)

    val tmp2: Either[String, Int] = getValue[Person, Int](Person("Beata", 6, "Malmö", books), "name")
    println(tmp2)

    val tmp3: Either[String, String] = getValue[Person, String](Person("Beata", 6, "Malmö", books), "name")
    println(tmp3)

    val tmp4: Either[String, String] = getValue[Person, String](Person("Beata", 6, "Malmö", books), "city")
    println(tmp4)

    val tmp5: Either[String, Integer] = getValue[Person, Integer](Person("Beata", 6, "Malmö", books), "age")
    println(tmp5)


    println("***********************")

    val personValidator =
      maxLength[Person]("name", 21) >=>
        minLength[Person]("name", 0) >=>
        maxValue[Person]("age", 121) >=>
        minValue[Person]("age", 0) >=>
        maxLength[Person]("city", 21) >=>
        minLength[Person]("city", 0)  //>=>
        // minLength[Person, List[String]]("books", 1)

    println("dsd".length)
    println(books.length)
    // println(personValidator.run(Person("NiklasNiklasNiklasNiklas", 46, "Malmö")))
    // println(personValidator.run(Person("", 120, "Malmö")))
    // println(personValidator.run(Person("Adam", 121, "Malmö")))
    // println(personValidator.run(Person("Adam", 120, "Malmöööööööööööööööööööööööööööööö")))
    println(personValidator.run(Person("Niklas", 47, "Malmö", books)))
  }

}
