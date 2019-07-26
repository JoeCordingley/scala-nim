package nim.imperative

import scala.math.min

object Nim {
  sealed trait Player
  case object PlayerOne extends Player
  case object PlayerTwo extends Player
  case class Winner(player: Player)

  def main(args: Array[String]): Unit = {
    println("how many stones do you wish to play with?")
    val number = readLine().toInt
    val winner = play(number, PlayerOne).player
    println(s"${playerString(winner)} is the winner!")
  }

  def play(stonesLeft: Int, player: Player): Winner = {
    val maxStones = min(3, stonesLeft)
    println(
      s"${playerString(player)}, there are $stonesLeft stones left, you may take a maximum of $maxStones stones, how many do you take?"
    )
    val stones = readLine().toInt
    val newTotal = stonesLeft - stones
    if (newTotal == 0) Winner(player) else play(newTotal, nextPlayer(player))
  }

  val playerString: Player => String = {
    case PlayerOne => "Player one"
    case PlayerTwo => "Player two"
  }

  val nextPlayer: Player => Player = {
    case PlayerOne => PlayerTwo
    case PlayerTwo => PlayerOne
  }

}
