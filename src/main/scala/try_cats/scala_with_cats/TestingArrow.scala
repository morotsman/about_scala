package try_cats.scala_with_cats

import cats._
import cats.implicits._
import cats.arrow.Arrow
import cats.data.Kleisli

object TestingArrow {

  def main(args: Array[String]): Unit = {
    println(meanAndVar(List(1, 2, 3, 4)))
  }

  val add = (a: Int) => a + 1

  val mul = (a: Int) => a * 2

  val sub = (a: Int) => a - 1

  println((add >>> mul) (2))

  // Adding more methods
  println((add >>> mul >>> sub) (2))

  val addInOption = (a: Int) => Option(a + 1)

  val mulInOption = (a: Int) => Option(a * 2)

  val subInOption = (a: Int) => Option(a - 1)

  val test1: Kleisli[Option, Int, Int] = Kleisli(addInOption).andThen(Kleisli(mulInOption))
  val test2: Kleisli[Option, Int, Int] = Kleisli(addInOption) >>> Kleisli(mulInOption)
  println("test2: " + test2.run(1))


  def combine[F[_, _] : Arrow, A, B, C](fab: F[A, B], fac: F[A, C]): F[A, (B, C)] = {
    val faabc: F[(A, A), (B, C)] = fab *** fac
    val faaa: F[A, (A, A)] = Arrow[F].lift((a: A) => (a, a));
    faaa >>> faabc
  }

  val mean: List[Int] => Double = {
    val sum: List[Int] => Int = (_: List[Int]).sum
    val length: List[Int] => Int = (_: List[Int]).size
    val sumAndLength: List[Int] => (Int, Int) = combine(sum, length)
    sumAndLength >>> { case (x, y) => x.toDouble / y }
  }

  val variance: List[Int] => Double =
  // Variance is mean of square minus square of mean
    combine(((_: List[Int]).map(x => x * x)) >>> mean, mean) >>> { case (x, y) => x - y * y }

  val meanAndVar: List[Int] => (Double, Double) = combine(mean, variance)


  // import cats.instances.int._    // for Monoid
  // import cats.instances.option._ // for Monoid

  Option(1) |+| Option(2)
  // res1: Option[Int] = Some(3)

  // import cats.instances.map._ // for Monoid

  val map1 = Map("a" -> 1, "b" -> 2)
  val map2 = Map("b" -> 3, "d" -> 4)

  map1 |+| map2
  // res3: Map[String,Int] = Map(b -> 5, d -> 4, a -> 1)

  // import cats.instances.tuple._  // for Monoid

  val tuple1 = ("hello", 123)
  val tuple2 = ("world", 321)

  val result = tuple1 |+| tuple2
  println(result)
  // res6: (String, Int) = (helloworld,444)

  println(Option(3).fproduct(i => ("a" + i)))

  val result2 = List((i: Int) => i * 3, (i: Int) => i * 4) <*> List(1, 2, 3)
  println(result2)


}
