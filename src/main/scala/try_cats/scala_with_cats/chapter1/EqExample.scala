package try_cats.scala_with_cats.chapter1

import cats._
import cats.implicits._

class EqExample {

  // bug
  List(1, 2, 3).map(Option(_)).filter(item => item == 1)

  val eqInt = Eq[Int]

  eqInt.eqv(123, 123)
  // res2: Boolean = true

  eqInt.eqv(123, 234)
  // res3: Boolean = false

  // eqInt.eqv(123, "234") will not compile

  123 === 123
  // res5: Boolean = true

  123 =!= 234
  // res6: Boolean = true

  // 123 === "123" will not compile

  //Some(1) === None will not compile
  (Some(1) : Option[Int]) === (None : Option[Int])
  Option(1) === Option.empty[Int]
  1.some === none[Int]
  1.some =!= none[Int]


  import java.util.Date
  import cats.instances.long._ // for Eq

  implicit val dateEq: Eq[Date] =
    Eq.instance[Date] { (date1, date2) =>
      date1.getTime === date2.getTime
    }

  val x = new Date() // now
  val y = new Date() // a bit later than now

  x === x
  // res13: Boolean = true

  x === y
  // res14: Boolean = false

}
