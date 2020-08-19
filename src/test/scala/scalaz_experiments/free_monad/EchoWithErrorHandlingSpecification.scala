package scalaz_experiments.free_monad

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import scalaz._
import scalaz_experiments.free_monad.EchoProgramWithErrorHandling.program

import scala.util.{Try, Failure, Success}

object EchoWithErrorHandlingSpecification extends Properties("Echo") {

  case class Buffers[A](in: List[A], out: List[A])

  type EchoState[A] = State[Buffers[Any], A]
  val pureCompilerNiceOutput: EchoA ~> EchoState = new (EchoA ~> EchoState) {
    override def apply[A](fa: EchoA[A]): EchoState[A] = fa match {
      case Read() => for {
        old <- State.get
        _ <- State.modify(addToOutput(old.in.head))
        _ <- State.modify(addToInput(old.in.tail))
      } yield Try(old.in.head)
      case PrintLn(output) => for {
        r <- State.modify(addToOutput(output))
      } yield Try(r)
      case Print(output) => for {
        r <- State.modify(addToOutput(output))
      } yield Try(r)
    }
  }

  def addToOutput[A](i: A)(s: Buffers[Any]): Buffers[Any] =
    s.copy(out = i :: s.out)

  def addToInput[A](i: List[A])(s: Buffers[Any]): Buffers[Any] =
    s.copy(in = i)

  property("echo") = forAll { (input: List[String]) =>
    val myInput = input.filter(_ != "q").appended("q")

    val result: (Buffers[Any], Try[Unit]) = program.foldMap(pureCompilerNiceOutput).run(Buffers(myInput, List()))

    val hello = List(
      "The great echo program!",
      "'q' to quit"
    )

    val goodbye = List(
      "Hope to see you again soon, goodbye!"
    )

    val output = myInput.flatMap(s => List("> ", s, s"You wrote: '$s'", ""))

    val expectedResult = hello ++ output ++ goodbye

    expectedResult == result._1.out.reverse
  }

  property("read error") = forAll { (input: List[String]) =>
    val myInput = input.filter(_ != "q").appended("q")

    val result: (Buffers[Any], Try[Unit]) = program.foldMap(readError).run(Buffers(myInput, List()))

    val hello = List(
      "The great echo program!",
      "'q' to quit"
    )

    val goodbye = List(
      "Hope to see you again soon, goodbye!"
    )

    val output = List("> ", "Oops, something has gone wrong: java.lang.RuntimeException: Failed to read", "")

    val expectedResult = hello ++ output ++ goodbye
    expectedResult == result._1.out.reverse
  }

  val readError: EchoA ~> EchoState = new (EchoA ~> EchoState) {
    override def apply[A](fa: EchoA[A]): EchoState[A] = fa match {
      case Read() => State { s =>
        (s, Try(throw new RuntimeException("Failed to read")).asInstanceOf[A])
      }
      case PrintLn(output) => for {
        r <- State.modify(addToOutput(output))
      } yield Try(r)
      case Print(output) => for {
        r <- State.modify(addToOutput(output))
      } yield Try(r)
    }
  }

  property("print error") = forAll { (input: List[String]) =>
    val myInput = input.filter(_ != "q").appended("q")

    val result: (Buffers[Any], Try[Unit]) = program.foldMap(printError).run(Buffers(myInput, List()))

    result._2 == Try(throw new RuntimeException("Failed to write"))
    List() == result._1.out.reverse
  }

  val printError: EchoA ~> EchoState = new (EchoA ~> EchoState) {
    override def apply[A](fa: EchoA[A]): EchoState[A] = fa match {
      case Read() => State { s =>
        (s, Try(throw new RuntimeException("Failed to write")).asInstanceOf[A])
      }
      case PrintLn(output) => State { s =>
        (s, Try(throw new RuntimeException("Failed to write")).asInstanceOf[A])
      }
      case Print(output) => State { s =>
        (s, Try(throw new RuntimeException("Failed to write")).asInstanceOf[A])
      }
    }
  }


}
