package net.degoes.zio

import net.degoes.zio.ZIOModel.ZIO.access
import zio._

/*
 * INTRODUCTION
 *
 * ZIO effects are immutable data values that model a possibly complex series
 * of async, concurrent, resourceful, and contextual computations.
 *
 * The only effect type in ZIO is called ZIO, and has three type parameters,
 * which permit accessing context from an enviornment (`R`), failing with a
 * value of a certain type (`E`), and succeeding with a value of a certain
 * type (`A`).
 *
 * Unlike Scala's Future, ZIO effects are completely lazy. All methods on ZIO
 * effects return new ZIO effects. No part of the workflow is executed until
 * one of the `unsafeRun*` functions are called.
 *
 * ZIO effects are transformed and combined using methods on the ZIO data type.
 * For example, two effects can be combined into a sequential workflow using
 * an operator called `zip`. Similarly, two effects can be combined into a
 * parallel workflow using an operator called `zipPar`.
 *
 * The operators on the ZIO data type allow very powerful, expressive, and
 * type-safe transformation and composition, while the methods in the ZIO
 * companion object allow building new effects from simple values (which are
 * not themselves effects).
 *
 * In this section, you will explore both the ZIO data model itself, as well
 * as the very basic operators used to transform and combine ZIO effects, as
 * well as a few simple ways to build effects.
 */

object ZIOModel {

  /**
   * EXERCISE
   *
   * Implement all missing methods on the ZIO companion object.
   */
  object ZIO {
    def succeed[A](a: => A): ZIO[Any, Nothing, A] = ZIO(_ => Right(a))

    def fail[E](e: => E): ZIO[Any, E, Nothing] = ZIO(_ => Left(e))

    def effect[A](sideEffect: => A): ZIO[Any, Throwable, A] = ZIO(_ => try Right(sideEffect) catch {
      case t: Throwable => Left(t)
    })

    def environment[R]: ZIO[R, Nothing, R] = ZIO(r => Right(r))

    def access[R, A](f: R => A): ZIO[R, Nothing, A] = ZIO.environment.map(f)

    def accessM[R, E, A](f: R => ZIO[R, E, A]): ZIO[R, E, A] = ZIO.environment.flatMap(f)
  }

  /**
   * EXERCISE
   *
   * Implement all missing methods on the ZIO class.
   */
  final case class ZIO[-R, +E, +A](run: R => Either[E, A]) { self =>
    def map[B](f: A => B): ZIO[R, E, B] = ZIO(r => run(r).map(f))

    def flatMap[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
      ZIO(r => self.run(r).fold(ZIO.fail(_), f).run(r))

    def zip[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, (A, B)] =
      ZIO(r =>
        for {
          a <- self.run(r)
          b <- that.run(r)
        } yield (a, b)
      )

    def either: ZIO[R, Nothing, Either[E, A]] =
      ZIO(r => Right(self.run(r)))

    def provide(r: R): ZIO[Any, E, A] =
      ZIO(_ => self.run(r))

    def orDie(implicit ev: E <:< Throwable): ZIO[R, Nothing, A] =
      ZIO(r => self.run(r).fold(throw _, Right(_)))
  }

  def putStrLn(line: String): ZIO[Any, Nothing, Unit] =
    ZIO.effect(println(line)).orDie

  val readLine: ZIO[Any, Nothing, String] =
    ZIO.effect(scala.io.StdIn.readLine()).orDie

  def unsafeRun[A](zio: ZIO[Any, Throwable, A]): A =
    zio.run(()).fold(throw _, a => a)

  /**
   * Run the following main function and compare the results with your
   * expectations.
   */
  def main(args: Array[String]): Unit =
    unsafeRun {
      putStrLn("Hello, what is your name?").flatMap(
        _ => readLine.flatMap(name => putStrLn(s"Your name is: $name"))
      )
    }
}

object ZIOTypes {
  type ??? = Nothing

  /**
   * EXERCISE
   *
   * Provide definitions for the ZIO type aliases below.
   */
  type Task[+A]     = ZIO[Any, Throwable, A]
  type UIO[+A]      = ZIO[Any, Nothing, A]
  type RIO[-R, +A]  = ZIO[R, Nothing, A]
  type IO[+E, +A]   = ZIO[Any, E, A]
  type URIO[-R, +A] = ZIO[R, Nothing, A]
}

object SuccessEffect extends App {
  import zio.console._

  /**
   * EXERCISE
   *
   * Using `ZIO.succeed`, create an effect that succeeds with a success
   * `ExitCode`.
   */
  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    ZIO.succeed(ExitCode.success)
}

object HelloWorld extends App {
  import zio.console._

  /**
   * EXERCISE
   *
   * Implement a simple "Hello World!" program using the effect returned by
   * `putStrLn`, using `ZIO#exitCode` method to transform the `putStrLn`
   * effect into another one that produces an exit code.
   */
  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    putStrLn("something").exitCode

}

object SimpleMap extends App {
  import zio.console._

  val readLine = getStrLn.orDie

  /**
   * EXERCISE
   *
   * Using `ZIO#map`, map the string success value of `readLine` into an
   * integer (the length of the string), and then further map that
   * into a constant exit code by using `ZIO#as`.
   */
  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    readLine.map(_.length).as(ExitCode.success)
}

object PrintSequenceZip extends App {
  import zio.console._

  /**
   * EXERCISE
   *
   * Using `zip`, compose a sequence of `putStrLn` effects to produce an effect
   * that prints three lines of text to the console.
   */
  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    putStrLn("first").zip(putStrLn("second")).zip(putStrLn("third"))
      .as(ExitCode.success)
}

object PrintSequence extends App {
  import zio.console._

  /**
   * EXERCISE
   *
   * Using `*>` (`zipRight`), compose a sequence of `putStrLn` effects to
   * produce an effect that prints three lines of text to the console.
   */
  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (putStrLn("first") *> putStrLn("second") *> putStrLn("third")).as(ExitCode.success)
}

object PrintReadSequence extends App {
  import zio.console._

  val readLine = getStrLn.orDie

  /**
   * EXERCISE
   *
   * Using `*>` (`zipRight`), sequentially compose a `putStrLn` effect, which
   * models printing out "Hit Enter to exit...", together with a `readLine`
   * effect, which models reading a line of text from the console.
   */
  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (putStrLn("Hit Enter to exit...") *> readLine).as(ExitCode.success)
}

object SimpleDuplication extends App {
  import zio.console._

  /**
   * EXERCISE
   *
   * In the following program, the expression `putStrLn("Hello again")`
   * appears three times. Factor out this duplication by introducing a new
   * value that stores the expression, and then referencing that variable
   * three times.
   */
  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = {
     val effect = putStrLn("Hello again")
    (putStrLn("Hello")
      *> effect *> effect *> effect).as(ExitCode.success)
  }
}

object FlatMap extends App {
  import zio.console._

  val readLine = getStrLn.orDie

  /**
   * EXERCISE
   *
   * The following program is intended to ask the user for their name, then
   * read their name, then print their name back out to the user. However,
   * the `zipRight` (`*>`) operator is not powerful enough to solve this
   * problem, because it does not allow a _subsequent_ effect to depend
   * on the success value produced by a _preceding_ effect.
   *
   * Solve this problem by using the `ZIO#flatMap` operator, which composes
   * a first effect together with a "callback", which can return a second
   * effect that depends on the success value produced by the first effect.
   */
  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    putStrLn("What is your name?") *>
      readLine.flatMap(name =>
      putStrLn(s"Your name is: $name") *>
      ZIO.succeed(ExitCode.success))
}

object PromptName extends App {
  import zio.console._

  val readLine = getStrLn.orDie

  /**
   * EXERCISE
   *
   * The following program uses a combination of `zipRight` (`*>`), and
   * `flatMap`. However, this makes the structure of the program harder
   * to understand. Replace all `zipRight` by `flatMap`, by ignoring the
   * success value of the left hand effect.
   */
  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    for {
      _ <- putStrLn("What is your name?")
      name <- readLine
      _ <- putStrLn(s"Your name is: $name")
    } yield ExitCode.success

  /**
   * EXERCISE
   *
   * Implement a generic "zipRight" that sequentially composes the two effects
   * using `flatMap`, but which succeeds with the success value of the effect
   * on the right-hand side.
   */
  def myZipRight[R, E, A, B](
    left: ZIO[R, E, A],
    right: ZIO[R, E, B]
  ): ZIO[R, E, B] =
    for {
      _ <- left
      r <- right
    } yield r
}

object ForComprehension extends App {
  import zio.console._

  val readLine = getStrLn.orDie

  /**
   * EXERCISE
   *
   * Rewrite the following program to use a `for` comprehension. Each line in
   * the for comprehension will be translated by Scala into a `flatMap`,
   * except for the final line, which will be translated into a `map`.
   */
  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    for {
      _ <- putStrLn("What is your name?")
      name <- readLine
      _ <- putStrLn(s"Your name is: $name")
    } yield ExitCode.success
}

object ForComprehensionBackward extends App {
  import zio.console._

  val readInt = getStrLn.flatMap(string => ZIO(string.toInt)).orDie

  /**
   * EXERCISE
   *
   * Rewrite the following program, which uses a `for` comprehension, to use
   * explicit `flatMap` and `map` methods. Note: each line of the `for`
   * comprehension will translate to a `flatMap`, except the final line,
   * which will translate to a `map`.
   */
  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = {
    putStrLn("How old are you?").flatMap { _ =>
      readInt.flatMap { age =>
        (
          if (age < 18) putStrLn("You are a kid!")
          else putStrLn("You are all grown up!")
        ).map(_ => ExitCode.success)
      }
    }
  }
}

object NumberGuesser extends App {
  import zio.console._
  import zio.random._

  val readLine = getStrLn.orDie

  def analyzeAnswer(random: Int, guess: String) =
    if (random.toString == guess.trim) putStrLn("You guessed correctly!")
    else putStrLn(s"You did not guess correctly. The answer was $random")

  /**
   * EXERCISE
   *
   * Choose a random number (using `nextInt`), and then ask the user to guess
   * the number (using `getStrLn`), feeding their response to `analyzeAnswer`,
   * above.
   */
  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    for {
      i <- nextInt
      guess <- readLine
      _ <- analyzeAnswer(i, guess)
    } yield ExitCode.success
}

object SingleSideEffect extends App {

  /**
   * EXERCISE
   *
   * Using ZIO.effect, convert the side-effecting of `println` into a pure
   * functional effect.
   */
  def myPrintLn(line: String): Task[Unit] = ZIO.effect(println(line)) // other possibility: Task((println(line))

  def run(args: List[String]) =
    myPrintLn("Hello World!").exitCode
}

object MultipleSideEffects extends App {

  /**
   * Using `ZIO.effect`, wrap Scala's `println` method to lazily convert it
   * into a functional effect, which describes the action of printing a line
   * of text to the console, but which does not actually perform the print.
   */
  def putStrLn(line: String): Task[Unit] = ZIO.effect(println(line))

  /**
   * Using `ZIO.effect`, wrap Scala's `scala.io.StdIn.readLine()` method to
   * lazily convert it into a functional effect, which describes the action
   * of printing a line of text to the console, but which does not actually
   * perform the print.
   */
  val getStrLn: Task[String] = ZIO.effect(scala.io.StdIn.readLine)

  def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      _    <- putStrLn("Hello, what is your name?")
      name <- getStrLn
      _    <- putStrLn(s"Good to meet you, $name!")
    } yield ()).exitCode
}
