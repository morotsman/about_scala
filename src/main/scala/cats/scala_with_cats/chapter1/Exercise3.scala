package cats.scala_with_cats.chapter1

import cats._
import cats.implicits._

object Exercise3 {

  final case class Cat(name: String, age: Int, color: String)

  implicit val catEq = new Eq[Cat] {
    override def eqv(x: Cat, y: Cat): Boolean =
      x.name === y.name && x.age === y.age && x.color === y.color
  }

  def main(args: Array[String]): Unit = {
    val cat1 = Cat("Micky", 10, "Black")
    val cat2 = Cat("Gustav", 10, "Orange")

    assert(cat1 === cat1)
    assert(cat1 =!= cat2)
    assert(cat2 === cat2)
  }



}
