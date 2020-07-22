package scalaz

import scalaz._
import Scalaz._

import scala.reflect.ClassTag

import scala.reflect.runtime.universe._
import scala.language.reflectiveCalls
import Validate._


object KleisliUsage {



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
