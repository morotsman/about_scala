package scalaz_experiments.free_monad

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import scalaz.Scalaz._
import scalaz._
import scalaz_experiments.free_monad.Echo.program

import scala.util.Try

object EchoSpecification extends Properties("Echo") {

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
        _ <- State.modify(addToOutput(output))
      } yield Try(())
      case Print(output) => for {
        _ <- State.modify(addToOutput(output))
      } yield Try(())
    }
  }

  def addToOutput[A](i: A)(s: Buffers[Any]): Buffers[Any] =
    s.copy(out = i :: s.out)

  def addToInput[A](i: List[A])(s: Buffers[Any]): Buffers[Any] =
    s.copy(in = i)
  
  property("echo") = forAll { (input: List[String]) =>
    val myInput = input.filter(_ != "q").appended("q")

    val result: (Buffers[Any], Try[Unit]) = program.foldMap(pureCompilerNiceOutput).run(Buffers(myInput, List()))

    val intro = List(
      "The great echo program!",
      "'q' to quit"
    )

    val goodbye = List(
      "Hope to see you again soon, goodbye!"
    )

    val output = myInput.flatMap(s => List("> ", s, s"You wrote: '$s'", ""))

    val expectedResult = intro ++ output ++ goodbye

    expectedResult == result._1.out.reverse
  }


}
