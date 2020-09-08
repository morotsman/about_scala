package scalaz_experiments.free_monad.candy3.pure

import org.scalatest.flatspec.AnyFlatSpec
import scalaz_experiments.free_monad.candy3.Types.InternalState
import scalaz_experiments.free_monad.candy3.interpreter.{StateIOInterpreter, StateMachineInterpreter}

class CliCandyProgramTest extends AnyFlatSpec {

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

  val invalidCommand = List(
    Right("j"),
    Right("Invalid request: j")
  )

  val noCandiesLeft = List(
    Right("c"),
    Right("Error when handling request: No candies left")
  )

  val locked = true
  val unlocked = false

  val initialMachine = MachineState(Some(0), locked, 20, 0)

  "A User" should "be able to quit" in {
    val state = InternalState[String](List(Right("q")), List[Either[Exception, String]](), Right(initialMachine): Either[Throwable, MachineState])

    val result = CliProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._1.out.reverse
    val actualState = result._1.machine

    val expectedOutput = welcome ++ help ++ quit
    assert(actualOutput == expectedOutput)

    val expectedState = Right(initialMachine)
    assert(actualState == expectedState)
  }

  "A User" should "be able to unlock" in {
    val state = InternalState[String](List(Right("c"), Right("q")), List[Either[Exception, String]](), Right(initialMachine): Either[Throwable, MachineState])

    val result = CliProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._1.out.reverse
    val actualState = result._1.machine

    val expectedOutput = welcome ++ help ++ coinDisposed ++ quit
    assert(actualOutput == expectedOutput)

    val expectedState = Right(initialMachine.copy(
      coins = initialMachine.coins + 1,
      locked = unlocked
    ))
    assert(actualState == expectedState)
  }

  "A User" should "be able to turn if a candy has been disposed" in {
    val state = InternalState[String](List(Right("c"), Right("t"), Right("q")), List[Either[Exception, String]](), Right(initialMachine): Either[Throwable, MachineState])

    val result = CliProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._1.out.reverse
    val actualState = result._1.machine

    val expectedOutput = welcome ++ help ++ coinDisposed ++ turn ++ quit
    assert(actualOutput == expectedOutput)

    val expectedState = Right(initialMachine.copy(
      candies = initialMachine.candies - 1,
      coins = initialMachine.coins + 1,
      locked = locked
    ))
    assert(actualState == expectedState)
  }

  "A User" should "not be able to dispose a coin if the machine is unlocked" in {
    val state = InternalState[String](List(Right("c"), Right("c"), Right("q")), List[Either[Exception, String]](), Right(initialMachine): Either[Throwable, MachineState])

    val result = CliProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._1.out.reverse
    val actualState = result._1.machine

    val expectedOutput = welcome ++ help ++ coinDisposed ++ coinRejected ++ quit
    assert(actualOutput == expectedOutput)

    val expectedState = Right(initialMachine.copy(
      coins = initialMachine.coins + 1,
      locked = unlocked
    ))
    assert(actualState == expectedState)
  }

  "A CandyProgram" should "accept a turn after a faulty input" in {
    val state = InternalState[String](List(Right("c"), Right("c"), Right("t"), Right("q")), List[Either[Exception, String]](), Right(initialMachine): Either[Throwable, MachineState])

    val result = CliProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._1.out.reverse
    val actualState = result._1.machine

    val expectedOutput = welcome ++ help ++ coinDisposed ++ coinRejected ++ turn ++ quit
    assert(actualOutput == expectedOutput)

    val expectedState = Right(initialMachine.copy(
      candies = initialMachine.candies - 1,
      coins = initialMachine.coins + 1,
      locked = locked
    ))
    assert(actualState == expectedState)
  }

  "A CandyProgram" should "reject a turn if no coin has been disposed" in {
    val state = InternalState[String](List(Right("t"), Right("q")), List[Either[Exception, String]](), Right(initialMachine): Either[Throwable, MachineState])

    val result = CliProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._1.out.reverse
    val actualState = result._1.machine

    val expectedOutput = welcome ++ help ++ turnRejected ++ quit
    assert(actualOutput == expectedOutput)

    assert(actualState == Right(initialMachine))
  }

  "A CandyProgram" should "be able to help" in {
    val state = InternalState[String](List(Right("h"), Right("q")), List[Either[Exception, String]](), Right(initialMachine): Either[Throwable, MachineState])

    val result = CliProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._1.out.reverse
    val actualState = result._1.machine

    val expectedOutput = welcome ++ help ++ showHelp ++ quit
    assert(actualOutput == expectedOutput)

    assert(actualState == Right(initialMachine))
  }

  "A CandyProgram" should "be able to show the current state" in {
    val state = InternalState[String](List(Right("s"), Right("q")), List[Either[Exception, String]](), Right(initialMachine): Either[Throwable, MachineState])

    val result = CliProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._1.out.reverse
    val actualState = result._1.machine

    val expectedOutput = welcome ++ help ++ currentState ++ quit
    assert(actualOutput == expectedOutput)

    assert(actualState == Right(initialMachine))
  }

  "A CandyProgram" should "reject an invalid command" in {
    val state = InternalState[String](List(Right("j"), Right("q")), List[Either[Exception, String]](), Right(initialMachine): Either[Throwable, MachineState])

    val result = CliProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._1.out.reverse
    val actualState = result._1.machine

    val expectedOutput = welcome ++ help ++ invalidCommand ++ quit
    assert(actualOutput == expectedOutput)

    assert(actualState == Right(initialMachine))
  }

  "A CandyProgram" should "reject a coin if all candies has been disposed" in {
    val initialMachine = MachineState(Some(0), locked, 0, 20)
    val state = InternalState[String](List(Right("c"), Right("q")), List[Either[Exception, String]](), Right(initialMachine): Either[Throwable, MachineState])

    val result = CliProgram.cliProgram.value.foldMap(interpreter).run(state.asInstanceOf[InternalState[Any]]).value
    val actualOutput = result._1.out.reverse
    val actualState = result._1.machine

    val expectedOutput = welcome ++ help ++ noCandiesLeft ++ quit
    assert(actualOutput == expectedOutput)

    assert(actualState == Right(initialMachine))
  }

}
