package scalaz_experiments.free_monad.candy3.pure

import org.scalatest.flatspec.AnyFlatSpec
import scalaz_experiments.free_monad.candy3.Types.InternalState
import scalaz_experiments.free_monad.candy3.interpreter.{StateIOInterpreter, StateMachineInterpreter}

class CandyProgramTest extends AnyFlatSpec {

  val interpreter = StateMachineInterpreter.stateMachineInterpreter() or StateIOInterpreter

  val welcome = List(
    Right("Welcome to the candy machine"),
  )

  val help = List(
    Right("Available commands"),
    Right("s - get current state of machine"),
    Right("c - insert a coin"),
    Right("t - turn"),
    Right("h - help"),
    Right("q - quit")
  )

  val quit = List(
    Right("q")
  )

  val coinDisposed = List(
    Right("c"),
    Right("Coin disposed, turn to get your candy!")
  )

  val coinRejected = List(
    Right("c"),
    Right("Error when handling request: A coin has already been disposed")
  )

  val turn = List(
    Right("t"),
    Right("Here is your candy!")
  )

  val turnRejected = List(
    Right("t"),
    Right("Error when handling request: No coin has been disposed")
  )

  val showHelp = List(
    Right("h"),
    Right("Available commands"),
    Right("s - get current state of machine"),
    Right("c - insert a coin"),
    Right("t - turn"),
    Right("h - help"),
    Right("q - quit")
  )

  val currentState = List(
    Right("s"),
    Right("MachineState(Some(0),true,20,0)")
  )

  val locked = true
  val unlocked = false

  "A CandyProgram" should "be able to quit" in {
    val state = InternalState[String](List(Right("q")), List[Either[Exception, String]](), Right(MachineState(Some(0), locked, 20, 0)): Either[Throwable, MachineState])

    val result = CandyProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._1.out.reverse
    val actualState = result._1.machine

    val expectedOutput = welcome ++ help ++ quit
    assert(actualOutput == expectedOutput)

    val expectedState = Right(MachineState(Some(0), locked, 20, 0))
    assert(actualState == expectedState)
  }

  "A CandyProgram" should "be able to unlock" in {
    val state = InternalState[String](List(Right("c"), Right("q")), List[Either[Exception, String]](), Right(MachineState(Some(0), locked, 20, 0)): Either[Throwable, MachineState])

    val result = CandyProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._1.out.reverse
    val actualState = result._1.machine

    val expectedOutput = welcome ++ help ++ coinDisposed ++ quit
    assert(actualOutput == expectedOutput)

    val expectedState = Right(MachineState(Some(0), unlocked, 20, 1))
    assert(actualState == expectedState)
  }

  "A CandyProgram" should "be able to turn" in {
    val state = InternalState[String](List(Right("c"), Right("t"), Right("q")), List[Either[Exception, String]](), Right(MachineState(Some(0), locked, 20, 0)): Either[Throwable, MachineState])

    val result = CandyProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._1.out.reverse
    val actualState = result._1.machine

    val expectedOutput = welcome ++ help ++ coinDisposed ++ turn ++ quit
    assert(actualOutput == expectedOutput)

    val expectedState = Right(MachineState(Some(0), locked, 19, 1))
    assert(actualState == expectedState)
  }

  "A CandyProgram" should "reject a coin if a coin already has been disposed" in {
    val state = InternalState[String](List(Right("c"), Right("c"), Right("q")), List[Either[Exception, String]](), Right(MachineState(Some(0), locked, 20, 0)): Either[Throwable, MachineState])

    val result = CandyProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._1.out.reverse
    val actualState = result._1.machine

    val expectedOutput = welcome ++ help ++ coinDisposed ++ coinRejected ++ quit
    assert(actualOutput == expectedOutput)

    val expectedState = Left(new IllegalStateException("A coin has already been disposed"))
    assert(actualState.toString == expectedState.toString)
  }

  "A CandyProgram" should "reject a turn if no coin has been disposed" in {
    val state = InternalState[String](List(Right("t"), Right("q")), List[Either[Exception, String]](), Right(MachineState(Some(0), locked, 20, 0)): Either[Throwable, MachineState])

    val result = CandyProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._1.out.reverse
    val actualState = result._1.machine

    val expectedOutput = welcome ++ help ++ turnRejected ++ quit
    assert(actualOutput == expectedOutput)

    val expectedState = Left(new IllegalStateException("No coin has been disposed"))
    assert(actualState.toString == expectedState.toString)
  }

  "A CandyProgram" should "be able to help" in {
    val state = InternalState[String](List(Right("h"), Right("q")), List[Either[Exception, String]](), Right(MachineState(Some(0), locked, 20, 0)): Either[Throwable, MachineState])

    val result = CandyProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._1.out.reverse
    val actualState = result._1.machine

    val expectedOutput = welcome ++ help ++ showHelp ++ quit
    assert(actualOutput == expectedOutput)

    val expectedState = Right(MachineState(Some(0), locked, 20, 0))
    assert(actualState.toString == expectedState.toString)
  }

  "A CandyProgram" should "be able to show the current state" in {
    val state = InternalState[String](List(Right("s"), Right("q")), List[Either[Exception, String]](), Right(MachineState(Some(0), locked, 20, 0)): Either[Throwable, MachineState])

    val result = CandyProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._1.out.reverse
    val actualState = result._1.machine

    actualOutput.foreach(println)
    val expectedOutput = welcome ++ help ++ currentState ++ quit
    assert(actualOutput == expectedOutput)

    val expectedState = Right(MachineState(Some(0), locked, 20, 0))
    assert(actualState.toString == expectedState.toString)
  }

}
