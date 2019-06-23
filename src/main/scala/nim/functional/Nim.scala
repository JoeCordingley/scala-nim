package nim.functional

import cats._
import cats.implicits._
import scala.math._
import scala.util.Try
import cats.data.StateT
import cats.effect.IO

sealed trait Player
case object PlayerOne extends Player
case object PlayerTwo extends Player

object Nim {
  type Move = Int
  type Max = Int
  type GetMove[F[_]] = (Player, Max) => F[Move]
  type Winner = Player

  val nextPlayer: Player => Player = {
    case PlayerOne => PlayerTwo
    case PlayerTwo => PlayerOne
  }

  def play[F[_]: Monad](getMove: GetMove[F], stones: Int): F[Winner] = {
    def inner(player: Player): StateT[F, Int, Winner] =
      for {
        _ <- getMoveAndUpdate(total => getMove(player, min(3, total)))
        newTotal <- StateT.get[F, Int]
        winner <- if (newTotal == 0) StateT.pure[F, Int, Winner](player)
        else inner(nextPlayer(player))
      } yield winner
    inner(PlayerOne).runA(stones)
  }

  def getMoveAndUpdate[F[_]: Monad](
      f: Status => F[Move]
  ): StateT[F, Int, Move] = StateT[F, Int, Move] { state =>
    f(state).map(move => (state - move, move))
  }
  type Status = Int

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption
  def parseInput(s: String, max: Int): Option[Move] =
    parseInt(s).filter(i => i <= max && i >= 1)
  def parseStart(s: String): Option[Int] = parseInt(s).filter(i => i >= 1)

  def retry[F[_]: Monad, A](fa: F[Option[A]]): F[A] = fa.flatMap {
    case None    => retry(fa)
    case Some(a) => Monad[F].pure(a)
  }

  type GetMoveWithStatus[F[_]] = (Player, Max, Status) => F[Move]

  def playWithUpdates[F[_]: Monad](
      update: GetMoveWithStatus[F],
      stones: Int
  ): F[Winner] = {
    val getMove = (player: Player, max: Max) =>
      getMoveAndUpdate(state => update(player, max, state))
    play[StateT[F, Int, ?]](getMove, stones).runA(stones)
  }

  def retryWithError[F[_]: Monad, A](
      normal: F[Option[A]],
      error: F[Option[A]]
  ): F[A] = {
    type Request = F[Option[A]]
    val tryAndThenSetError: StateT[F, Request, Option[A]] =
      StateT[F, Request, Option[A]](f => f.map(o => (error, o)))
    retry[StateT[F, Request, ?], A](tryAndThenSetError).runA(normal)
  }

  val startString = "how many stones do you wish to play with?"
  val errorString = "invalid input"
  def turnString(player: Player, max: Int, status: Int) =
    s"${playerString(player)}, there are $status stones left, you may take a maximum of $max stones, how many do you take?"
  def retryWithErrorIO[A](f: IO[Option[A]]): IO[A] =
    retryWithError(f, print(errorString) *> f)
  def print(s: String): IO[Unit] = IO(println(s))
  val read: IO[String] = IO(readLine)

  val getStartingStones: IO[Int] = retryWithErrorIO {
    print(startString) *> read.map(parseStart)
  }
  def winnerString(winner: Winner) = s"${playerString(winner)} wins!"

  val playerString: Player => String = {
    case PlayerOne => "Player one"
    case PlayerTwo => "Player two"
  }
  val getTurn: GetMoveWithStatus[IO] = (player, max, status) =>
    retryWithErrorIO {
      print(turnString(player, max, status)) *> read.map(parseInput(_, max))
    }
  def announceWinner(winner: Winner): IO[Unit] = print(winnerString(winner))

  def playIO(stones: Int): IO[Winner] = playWithUpdates(getTurn, stones)
  val gameIO: IO[Unit] = getStartingStones >>= playIO >>= announceWinner
  def main(args: Array[String]): Unit = {
    gameIO.unsafeRunSync
  }

}
