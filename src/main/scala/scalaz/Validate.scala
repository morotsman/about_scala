package scalaz

import scala.reflect.ClassTag
import scala.reflect.ClassTag

import scala.reflect.runtime.universe._

object Validate {
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

  def maxLength[T](field: String, max: Int)(implicit ct: ClassTag[T], tt: TypeTag[T], m: Manifest[T]): Validator[T] = {
    validateField(field)
    (t: T) => {
      for {
        value <- getValue[T, String](t, field)
        result <- if (value.length < max) Right(t) else Left(s"Length on $field was ${value.length} but it must be less then $max")
      } yield result
    }
  }

  def validateField[T](field: String)(implicit m: Manifest[T]) = {
    val fieldExists = manifest[T].runtimeClass.getMethods.exists(m => m.getName == field)
    if (!fieldExists) throw new RuntimeException(s"Could not find a field called $field")
  }

  private def getType[T](field: String)(implicit m: Manifest[T]) = {
    manifest[T].runtimeClass.getMethods.filter(m => m.getName == field).map(m => m.getAnnotatedReturnType).head
  }

  def minLength[T](field: String, min: Int)(implicit ctf: ClassTag[T], ttf: TypeTag[T], m: Manifest[T]): Validator[T] = {
    validateField(field)
    (t: T) => {
      for {
        value <- getValue[T, String](t, field)
        result <- if (value.length > min) Right(t) else Left(s"Length on $field was ${value.length} but it most be greater then $min")
      } yield result
    }
  }

  def maxValue[T](field: String, max: Int)(implicit ct: ClassTag[T], tt: TypeTag[T], m: Manifest[T]): Validator[T] = {
    validateField(field)
    (t: T) => {
      for {
        value <- getValue[T, Integer](t, field)
        result <- if (value < max) Right(t) else Left(s"Value on $field was $value but it must be less then $max")
      } yield result
    }
  }

  def minValue[T](field: String, min: Int)(implicit ct: ClassTag[T], tt: TypeTag[T], m: Manifest[T]): Validator[T] = {
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
}
