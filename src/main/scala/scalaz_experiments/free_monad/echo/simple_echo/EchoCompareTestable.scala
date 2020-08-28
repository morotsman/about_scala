package scalaz_experiments.free_monad.echo.simple_echo

import scala.io.StdIn

trait IO {
  def printLn(s: String)

  def print(s: String)

  def read(): String
}

class EchoCompareTestable(io: IO) {

  def program(): Unit = {
    welcome
    loop
  }

  private def welcome(): Unit = {
    io.printLn("The great echo program!")
    io.printLn("'q' to quit")
  }

  private def loop: Unit = {
    val input = readFromPrompt
    echo(input)
    if (quit(input)) goodbye else loop
  }

  private def readFromPrompt: String = {
    io.print("> ")
    io.read()
  }

  private def echo(input: String): Unit = {
    io.printLn(s"You wrote: ${input}")
    io.printLn("")
  }

  private def quit(st: String): Boolean =
    st == "q"

  private def goodbye: Unit =
    io.printLn("Hope to see you again soon, goodbye!")

}

object Test {

  object MyIO extends IO {
    override def printLn(s: String): Unit = println(s)

    override def print(s: String): Unit = System.out.print(s)

    override def read(): String = StdIn.readLine()
  }

  def main(args: Array[String]): Unit = {
    new EchoCompareTestable(MyIO).program()
  }
}
