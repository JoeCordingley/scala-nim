package nim

import cats._
import cats.implicits._
import scala.math._

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
    def inner(player: Player, stonesLeft: Int): F[Winner] =
      if (stonesLeft == 0) Monad[F].pure(player)
      else
        for {
          move <- getMove(player, min(3, stonesLeft))
          winner <- inner(nextPlayer(player), stonesLeft - move)
        } yield winner
    inner(PlayerOne, stones)
  }

}
