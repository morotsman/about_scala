package scalaz_experiments.free_monad.echo.simple_echo

import scala.io.StdIn

object EchoCompare {

  def program(): Unit = {
    welcome
    loop
  }

  def welcome(): Unit = {
    println("The great echo program!")
    println("'q' to quit")
  }

  def loop: Unit = {
    val input = readFromPrompt
    echo(input)
    if (quit(input)) goodbye else loop
  }

  def readFromPrompt: String = {
    print("> ")
    StdIn.readLine
  }

  def echo(input: String): Unit = {
    println(s"You wrote: ${input}")
    println("")
  }

  private def quit(st: String): Boolean =
    st == "q"

  private def goodbye: Unit =
    println("Hope to see you again soon, goodbye!")


  def main(args: Array[String]): Unit = {
    program()
  }

}
