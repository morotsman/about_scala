package scalaz_experiments.free_monad.candy3.pure

import cats.data.EitherT
import cats.free.Free
import scalaz_experiments.free_monad.candy3.pure.Request._
import scalaz_experiments.free_monad.candy3.pure.algebra.IO

trait CliCandyProgram extends Program {

  def cliProgram(eventHandler: Request => Program[MachineState])(implicit I: IO[CandyMachine]): Program[Unit] = {

    def main(): Program[Unit] = (for {
      _ <- write("Welcome to the candy machine")
      _ <- showCommands
      _ <- doWhileM(processInput)(input => input != QuitRequest())
    } yield ())

    def showCommands: Program[Unit] = for {
      _ <- write("Available commands")
      _ <- write("s - get current state of machine")
      _ <- write("c - insert a coin")
      _ <- write("t - turn")
      _ <- write("h - help")
      _ <- write("q - quit")
    } yield ()

    def doWhileM[A](p: Program[A])(expr: => A => Boolean): Program[Unit] = for {
      a <- p
      _ <- if (expr(a)) doWhileM(p)(expr) else noop
    } yield ()

    def processInput: Program[Request] = for {
      request <- getRequest
      _ <- handleRequest(request)
    } yield request

    def getRequest: Program[Request] = (for {
      input <- read[String]()
      request <- toRequest(input)
    } yield request).recoverWith(e => handleInvalidRequest(e))

    def toRequest(s: String): Program[Request] = {
      val result = if (s == "c")
        Right(InsertCoin(0L))
      else if (s == "t")
        Right(Turn(0L))
      else if (s == "q")
        Right(QuitRequest())
      else if (s == "s")
        Right(GetMachineState(0))
      else if (s == "h")
        Right(HelpRequest())
      else
        Left(new IllegalArgumentException(s"Invalid request: $s"))

      pure(result)
    }

    def handleInvalidRequest(e: Throwable): Program[Request] = for {
      _ <- write(e.getMessage)
      r <- getRequest
    } yield r

    def handleRequest(request: Request): Program[Unit] = (request match {
      case QuitRequest() => noop
      case HelpRequest() => showCommands
      case CreateMachine(_) => for {
        _ <- eventHandler(request)
      } yield ()
      case GetMachineState(_) => for {
        m <- eventHandler(request)
        _ <- write(m.toString)
      } yield ()
      case InsertCoin(_) => for {
        _ <- eventHandler(request)
        _ <- write("Coin disposed, turn to get your candy!")
      } yield ()
      case Turn(_) => for {
        _ <- eventHandler(request)
        _ <- write("Here is your candy!")
      } yield ()
    }).recoverWith(e => write(s"Error when handling request: ${e.getMessage}"))

    def noop: Program[Unit] = pure(Right(()))

    def pure[A](v: Either[Throwable, A]): Program[A] = {
      def pureFreeProgram[A](v: A): FreeProgram[A] = Free.pure[CandyMachine, A](v)
      EitherT(pureFreeProgram(v))
    }

    def read[A](): Program[A] = EitherT(I.read[A]())

    def write[A](s: A): Program[Unit] = EitherT(I.write(s))

    main()
  }
}
