package nim

import cats.effect.IO
import cats.Show
import cats.implicits._

object Nim1 {


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

  val initialStones = 12

  def nim: IO[Unit] = {
    def play(stones: Int, player: Player): IO[Unit] =
      for {
        _ <- announceStonesLeft(stones)
        move <- getMove(player)
        stonesLeft = stones - move
        _ <-
          if (stonesLeft == 0) announceWinner(player)
          else play(stonesLeft, nextPlayer(player))
      } yield ()
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
