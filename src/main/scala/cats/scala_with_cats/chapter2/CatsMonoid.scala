package cats.scala_with_cats.chapter2

import cats.Monoid
import cats.instances.string._ // for Monoid
import cats.instances.int._ // for Monoid
import cats.instances.option._ // for Monoid
import cats.syntax.semigroup._ // for |+|

object CatsMonoid {

  Monoid[String].combine("Hi ", "there")

  Monoid[String].empty

  Monoid[Int].combine(32, 10)

  val a = Option(22)

  val b = Option(20)

  Monoid[Option[Int]].combine(a, b)

  val stringResult = "Hi " |+| "there" |+| Monoid[String].empty

  import cats.instances.int._ // for Monoid

  val intResult = 1 |+| 2 |+| Monoid[Int].empty

}

object Excersice1 {
  def add(items: List[Int]): Int = Monoid[Int].combineAll(items)

  def add2(items: List[Int]): Int =
    items.foldLeft(Monoid[Int].empty)(_ |+| _)
}

object Excersice2 {
  def add[A](items: List[A])(implicit monoid: Monoid[A]): A =
    items.foldLeft(monoid.empty)(_ |+| _)


  def main(args: Array[String]): Unit = {
    println(add(List(1, 2, 3)))

    println(add(List(Some(1), None, Some(2), None, Some(3))))

    // add(List(Some(1), Some(2), Some(3)))

    case class Order(totalCost: Double, quantity: Double)

    implicit val orderMonoid = new Monoid[Order] {
      override def empty: Order = Order(0, 0)

      override def combine(x: Order, y: Order): Order = Order(
        x.totalCost + y.totalCost,
        x.quantity + y.quantity
      )
    }

   println(add(List(Order(2,2), Order(3,3))))
  }



}
