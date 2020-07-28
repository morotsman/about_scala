package scalaz

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

final case class ValidationException(message: String) extends Exception(message)

object Validate {

  def apply[T](v: FieldValidator[T]): Kleisli[({type f[x] = Either[ValidationException, x]})#f, T, T] =
    Kleisli(v)

  type FieldValidator[T] = T => Either[ValidationException, T]

  def maxLengthString[T](max: Int): Validator[T, String] =
    Validator[T, String](
      v => v.length <= max,
      (t, v, f) => s"Length on $f was ${v.length} but it must be less or equal to $max for $t"
    )

  // TODO For some reason FT <: Any {def size: Int} doesn't work with String, revisit this when you know more!
  def minLengthString[T](min: Int): Validator[T, String] =
    Validator[T, String](
      v => v.length >= min,
      (t, v, f) => s"Length on $f was ${v.length} but it must be greater or equal to $min for $t"
    )

  final class MinLengthHelper[FT <: Any {def size: Int}] {
    def apply[T](min: Int): Validator[T, FT] =
      Validator[T, FT](
        v => v.size >= min,
        (t, v, f) => s"Length on $f was ${v.size} but it must be greater or equal to $min for $t"
      )
  }

  def minLength[FT <: Any {def size: Int}] = new MinLengthHelper[FT]

  final class MaxLengthHelper[FT <: Any {def size: Int}] {
    def apply[T](max: Int): Validator[T, FT] =
      Validator[T, FT](
        v => v.size <= max,
        (t, v, f) => s"Length on $f was ${v.size} but it must be less or equal to $max for $t"
      )
  }

  def maxLength[FT <: Any {def size: Int}] = new MaxLengthHelper[FT]

  def maxValue[T](max: Int): Validator[T, Integer] =
    Validator[T, Integer](
      v => v <= max,
      (t, v, f) => s"Value on $f was $v but it must be less or equal to $max for $t"
    )

  def minValue[T](min: Int): Validator[T, Integer] =
    Validator[T, Integer](
      value => value >= min,
      (t, v, f) => s"Value on $f was $v but it must be greater or equal to $min for $t"
    )

  case class Validator[T, FT](op: FT => Boolean, errorMessage: (T, FT, String) => String) {
    def on(fieldName: String)(implicit ct: ClassTag[T], tt: TypeTag[T], m: Manifest[T], ctf: ClassTag[FT], ttf: TypeTag[FT]): FieldValidator[T] = {
      validateField(fieldName)
      (t: T) => {
        for {
          value <- getValue[T, FT](t, fieldName)
          result <- if (op(value)) Right(t) else Left(ValidationException(errorMessage(t, value, fieldName)))
        } yield result
      }
    }

    private def validateField[T](field: String)(implicit m: Manifest[T]): Unit = {
      val fieldExists = manifest[T].runtimeClass.getMethods.exists(m => m.getName == field)
      if (!fieldExists) throw new RuntimeException(s"Could not find a field called $field")
    }

    private def getValue[I, O](obj: I, field: String)(implicit ct: ClassTag[I], tt: TypeTag[I], ct1: ClassTag[O], tt1: TypeTag[O]): Either[ValidationException, O] = {
      try {
        val symbol = typeOf[I].member(TermName(field)).asMethod
        val m = runtimeMirror(obj.getClass.getClassLoader)
        val im = m.reflect(obj)
        val value: O = cast[O](im.reflectMethod(symbol).apply())
        Right(value)
      } catch {
        case e: ClassCastException => Left(ValidationException(s"Wrong type on field $field:  $e"))
        case e: ScalaReflectionException => Left(ValidationException(s"Could not find a field called $field: $e"))
      }
    }

    // https://stackoverflow.com/questions/6686992/scala-asinstanceof-with-parameterized-types
    private def cast[A <: Any : Manifest](a: Any): A =
      manifest[A].runtimeClass.cast(a).asInstanceOf[A]
  }

}
