package nim
import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Gen
import nim.Nim._
import cats.data.WriterT
import cats.implicits._
import org.scalacheck.cats.implicits._

object NimCheck extends Properties("Nim") {

  type TestContext[A] = WriterT[Gen, List[Record], A]

  case class Record(player: Player, maxStones: Int, move: Move)

  def moveGen(maxStones: Int): Gen[Move] = Gen.choose(1, maxStones)

  val written: (Player, Int) => TestContext[Move] = (player, maxStones) =>
    for {
      move <- WriterT.liftF[Gen, List[Record], Move](moveGen(maxStones))
      _ <- WriterT.tell[Gen, List[Record]](
        List(Record(player, maxStones, move)))
    } yield move

  case class RecordedGame(winner: Winner,
                          records: List[Record],
                          numberOfStones: Int)

  val recordedGameGen: Gen[RecordedGame] = for {
    numberOfStones <- Gen.choose(1, 100)
    (records, winner) <- play[TestContext](written, numberOfStones).run
  } yield RecordedGame(winner, records, numberOfStones)

  val recordedPlayers: Gen[List[Player]] =
    recordedGameGen.map(_.records.map(_.player))

  property("players should be asked in sequence") =
    forAllNoShrink(recordedPlayers) { players =>
      val playerSequence: Stream[Player] =
        Stream.continually(Stream(PlayerOne, PlayerTwo)).flatten
      playerSequence.startsWith(players)
    }

  val numberOfPlayersAndRecordedMoves: Gen[(Int, List[Int])] =
    recordedGameGen.map(game => (game.numberOfStones, game.records.map(_.move)))

  property("play should end when all stones have been taken") =
    forAllNoShrink(numberOfPlayersAndRecordedMoves) {
      case (numberOfStones, moves) => numberOfStones == moves.sum
    }

  val maxStonesRecorded: Gen[Int] = for {
    recordedGame <- recordedGameGen
    record <- Gen.oneOf(recordedGame.records)
  } yield record.maxStones

  property("player should be given a max of between 1 and 3 stones") =
    forAllNoShrink(maxStonesRecorded) { max =>
      max >= 1 && max <= 3
    }

  val maxStonesAndStonesLeft: Gen[(Int, Int)] = for {
    recordedGame <- recordedGameGen
    numberOfMoves = recordedGame.records.length
    moveNumber <- Gen.choose(1, numberOfMoves)
  } yield {
    val (movesBefore, thisMove :: _) =
      recordedGame.records.splitAt(moveNumber - 1)
    val numberOfStonesTakenThusFar = movesBefore.map(_.move).sum
    val numberOfStonesLeft = recordedGame.numberOfStones - numberOfStonesTakenThusFar
    (thisMove.maxStones, numberOfStonesLeft)
  }

  property(
    "player should be given a max that is no more than the stones left") =
    forAllNoShrink(maxStonesAndStonesLeft) {
      case (maxStones, stonesLeft) => maxStones <= stonesLeft
    }

  val winnerAndLastMove: Gen[(Winner, Player)] =
    recordedGameGen.map(game => (game.winner, game.records.last.player))
  val otherPlayer: Player => Player = {
    case PlayerOne => PlayerTwo
    case PlayerTwo => PlayerOne
  }

  property(
    "play returns the other player from the last to move as the winner") =
    forAllNoShrink(winnerAndLastMove) {
      case (winner, lastToMove) => winner == otherPlayer(lastToMove)
    }

}
