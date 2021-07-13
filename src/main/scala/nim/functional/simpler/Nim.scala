package nim.functional.simpler

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

  def maxStones(total: Int): Int = min(3, total)

  def play[F[_]: Monad](getMove: GetMove[F], startingStones: Int): F[Winner] = {
    def inner(player: Player, total: Int): F[Winner] =
      for {
        move <- getMove(player, maxStones(total))
        newTotal = total - move
        winner <-
          if (newTotal == 0) Monad[F].pure(player)
          else inner(nextPlayer(player), newTotal)
      } yield winner
    inner(PlayerOne, startingStones)
  }

  type Status = Int

  type GetMoveWithStatus[F[_]] = (Player, Status) => F[Move]

  def playWithUpdates[F[_]: Monad](
      getMoveWithStatus: GetMoveWithStatus[F],
      startingStones: Int
  ): F[Winner] = {
    val getMove = (player: Player, _: Max) =>
      StateT[F, Int, Move] { state =>
        getMoveWithStatus(player, state).map(move => (state - move, move))
      }
    play[StateT[F, Int, ?]](getMove, startingStones).runA(startingStones)
  }

  def retry[F[_]: Monad, A](fa: F[Option[A]]): F[A] =
    fa.flatMap {
      case None    => retry(fa)
      case Some(a) => Monad[F].pure(a)
    }

  def retryWithError[F[_]: Monad, A](
      normal: F[Option[A]],
      error: F[Option[A]]
  ): F[A] = {
    type FState[X] = StateT[F, F[Option[A]], X]
    val tryAndThenSetError: FState[Option[A]] =
      StateT(f => f.map(o => (error, o)))
    retry[FState, A](tryAndThenSetError).runA(normal)
  }

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption
  def parseInput(s: String, max: Int): Option[Move] =
    parseInt(s).filter(i => i <= max && i >= 1)
  def parseStart(s: String): Option[Int] = parseInt(s).filter(i => i >= 1)

  def print(s: String): IO[Unit] = IO(println(s))
  val read: IO[String] = IO(readLine)

  val startString = "how many stones do you wish to play with?"
  val errorString = "invalid input"
  def turnString(player: Player, max: Int, status: Int) =
    s"${playerString(player)}, there are $status stones left, you may take a maximum of $max stones, how many do you take?"

  val getStartingStones: IO[Int] = retryWithError(
    print(startString) *> read.map(parseStart),
    print(errorString) *> read.map(parseStart)
  )

  def winnerString(winner: Winner) = s"${playerString(winner)} wins!"

  val playerString: Player => String = {
    case PlayerOne => "Player one"
    case PlayerTwo => "Player two"
  }

  val getTurn: GetMoveWithStatus[IO] = (player, status) => {
    val max = maxStones(status)
    val readInput = read.map(parseInput(_, max))
    retryWithError(
      print(turnString(player, max, status)) *> readInput,
      print(errorString) *> readInput
    )
  }
  def announceWinner(winner: Winner): IO[Unit] = print(winnerString(winner))

  def playIO(stones: Int): IO[Winner] = playWithUpdates(getTurn, stones)
  val gameIO: IO[Unit] = getStartingStones >>= playIO >>= announceWinner
  def main(args: Array[String]): Unit = gameIO.unsafeRunSync

}
