package nim

import cats.effect.IO
import cats.{Show, Monad}
import cats.implicits._

object Nim2 {


  sealed trait Player
  case object PlayerOne extends Player
  case object PlayerTwo extends Player

  def nextPlayer: Player => Player = {
    case PlayerOne => PlayerTwo
    case PlayerTwo => PlayerOne
  }

  implicit val showPlayer: Show[Player] = {
    case PlayerOne => "Player One"
    case PlayerTwo => "Player One"
  }

  type Move = Int
  trait GetMove[F[_]] {
    def apply(player: Player, stonesLeft: Int): F[Move]
  }

  val initialStones = 12

  case class Winner(player: Player)

  def nim[F[_]: Monad](getMove: GetMove[F]): F[Winner] = {
    def play(stones: Int, player: Player): F[Winner] = for {
      move <- getMove(player, stones)
      stonesLeft = stones - move
      winner <- if (stonesLeft == 0) Winner(player).pure[F] else play(stonesLeft, nextPlayer(player))
    } yield winner
    play(initialStones, PlayerOne)
  }

  def announceStonesLeft(stones: Int): IO[Unit] =
    IO(println(s"Stones left:  $stones"))

  def getMove(player: Player): IO[Int] =
    for {
      _ <- IO(println(show"$player, what is your move?"))
      move <- IO(readInt)
    } yield move

  def announceWinner(player: Player): IO[Unit] =
    IO(println(show"$player wins!"))

}
