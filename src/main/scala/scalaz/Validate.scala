package scalaz

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

object Validate {
  type Validator[T] = T => Either[String, T]

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

  // TODO For some reason minLength doesn't work with String, revisit this when you know more!
  def minLengthString[T](field: String, min: Int)(implicit ctf: ClassTag[T], ttf: TypeTag[T], m: Manifest[T]): Validator[T] = {
    validateField(field)
    (t: T) => {
      for {
        value <- getValue[T, String](t, field)
        result <- if (value.length > min) Right(t) else Left(s"Length on $field was ${value.length} but it most be greater then $min")
      } yield result
    }
  }

  final class MinLengthHelper[FT <: Any {def size: Int}] {
    def apply[T](field: String, min: Int)(implicit ctf: ClassTag[T], ttf: TypeTag[T], m: Manifest[T], ctft: ClassTag[FT], ttft: TypeTag[FT]): Validator[T] = {
      validateField(field)
      (t: T) => {
        for {
          value <- getValue[T, FT](t, field)
          result <- if (value.size > min) Right(t) else Left(s"Length on $field was ${value.size} but it most be greater then $min")
        } yield result
      }
    }
  }

  def minLength[FT <: Any {def size: Int}] = new MinLengthHelper[FT]

  // https://stackoverflow.com/questions/3508077/how-to-define-type-disjunction-union-types
  type ¬[A] = A => Nothing
  type ∨[T, U] = ¬[¬[T] with ¬[U]]
  type ¬¬[A] = ¬[¬[A]]
  type |∨|[T, U] = { type λ[X] = ¬¬[X] <:< (T ∨ U) }


  type hasSize = {def size: Int} |∨| String

  def length[T : hasSize#λ](t : T) = t match {
    case s : String => s.length
    case l : {def size: Int} => l.size;
  }


  def maxValue[T](field: String, max: Int)(implicit ct: ClassTag[T], tt: TypeTag[T], m: Manifest[T]): Validator[T] =
    intValidation(field, max, v => v < max, v => s"Value on $field was $v but it must be less then $max")

  def minValue[T](field: String, min: Int)(implicit ct: ClassTag[T], tt: TypeTag[T], m: Manifest[T]): Validator[T] =
    intValidation(field, min, value => value > min, v => s"Value on $field was $v but it must be greater then $min")

  private def intValidation[T](field: String, max: Int, op: Int => Boolean, errorMessage: Int => String)(implicit ct: ClassTag[T], tt: TypeTag[T], m: Manifest[T]): Validator[T] = {
    validateField(field)
    (t: T) => {
      for {
        value <- getValue[T, Integer](t, field)
        result <- if (op(value)) Right(t) else Left(errorMessage(value))
      } yield result
    }
  }

  private def validateField[T](field: String)(implicit m: Manifest[T]): Unit = {
    val fieldExists = manifest[T].runtimeClass.getMethods.exists(m => m.getName == field)
    if (!fieldExists) throw new RuntimeException(s"Could not find a field called $field")
  }
}
