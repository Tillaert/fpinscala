import fpinscala.iomonad.Monad

case class Player(name: String, score: Int)

val player_1 = Player("player_1", 1000)
val player_2 = Player("player_2", 800)
val player_3 = Player("player_3", 800)

object FirstTry {
  def contest(p1: Player, p2: Player): Unit =
    if (p1.score > p2.score)
      println(s"${p1.name} is the winner!")
    else if (p2.score > p1.score)
      println(s"${p2.name} is the winner!")
    else
      println("It's a draw.")


}

FirstTry.contest(player_1, player_2)
FirstTry.contest(player_3, player_1)
FirstTry.contest(player_2, player_3)


object SecondTry {
  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None

  def contest(p1: Player, p2: Player): Unit = winner(p1, p2) match {
    case Some(Player(name, _)) => println(s"$name is the winner!")
    case None => println("It's a draw.")
  }
}

SecondTry.winner(player_1, player_2)
SecondTry.winner(player_3, player_1)
SecondTry.winner(player_2, player_3)

SecondTry.contest(player_1, player_2)
SecondTry.contest(player_3, player_1)
SecondTry.contest(player_2, player_3)

object WithWinnerMsg {
  def winnerMsg(p: Option[Player]): String = p map {
    case Player(name, _) => s"$name is the winner!"
  } getOrElse "It's a draws."

  def contest(p1: Player, p2: Player): Unit =
    println(winnerMsg(SecondTry.winner(p1, p2)))
}

WithWinnerMsg.contest(player_1, player_2)
WithWinnerMsg.contest(player_3, player_1)
WithWinnerMsg.contest(player_2, player_3)

object SimpleIO {

  trait IO {
    def run(): Unit
  }

  def PrintLine(msg: String): IO =
    new IO {
      def run() = println(msg)
    }

  def contest(p1: Player, p2: Player): IO =
    PrintLine(WithWinnerMsg.winnerMsg(SecondTry.winner(p1, p2)))
}

SimpleIO.contest(player_1, player_2).run()
SimpleIO.contest(player_3, player_1).run()
SimpleIO.contest(player_2, player_3).run()

object SimpleIO2 {
  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  trait IO[A] {
    self =>
    def run: A

    def map[B](f: A => B): IO[B] =
      new IO[B] {
        def run = f(self.run)
      }

    def flatMap[B](f: A => IO[B]): IO[B] =
      new IO[B] {
        def run = f(self.run).run
      }
  }

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] {
      def run = a
    }

    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f

    def apply[A](a: => A): IO[A] = unit(a)
  }

  def ReadLine: IO[String] = IO("30") // Mock stdin


  def PrintLine(msg: String): IO[Unit] = IO {
    println(msg)
  }

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  val echo = ReadLine.flatMap(PrintLine)
  val readInt = ReadLine.map(_.toInt)
  val readInts = for {
    a <- readInt
    b <- readInt
  } yield (a, b)


}

SimpleIO2.converter.run

SimpleIO2.echo.run

SimpleIO2.readInt.run

SimpleIO2.readInts.run

try {
  SimpleIO2.IO.forever(SimpleIO2.PrintLine("Still going...")).run
} catch {
  case (e: StackOverflowError) => println("Caught stackoverflow error.")
}

object StackSafeIO {

  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f)

    def map[B](f: A => B): IO[B] =
      flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends IO[A]

  case class Suspend[A](resume: () => A) extends IO[A]

  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]


  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] {
      def run = a
    }

    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f

    def apply[A](a: => A): IO[A] = unit(a)
  }

  def printLine(s: String): IO[Unit] =
    Suspend(() => println(s))

  @annotation.tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y.flatMap (a => g(a) flatMap f))
    }
  }
}

StackSafeIO.run(StackSafeIO.printLine("StackSafeIO"))

val f: Int => StackSafeIO.IO[Int] = (x: Int) => StackSafeIO.Return(x)

val g = List.fill(100000)(f).foldLeft(f) {
  (a,b) => x => StackSafeIO.Suspend(() => ()).flatMap { _ => a(x).flatMap(b)}
}

StackSafeIO.run(g(0))

StackSafeIO.run(g(42))

