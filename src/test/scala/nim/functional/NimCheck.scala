package nim.functional

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalacheck.Gen
import nim.functional.Nim._
import cats.data.WriterT
import cats.data.State
import cats.data.Chain
import cats.implicits._
import org.scalacheck.cats.implicits._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary
import cats._
import scala.math._

object NimCheck extends Properties("Nim") {

  type TestContext[A] = WriterT[Gen, List[Record], A]

  case class Record(player: Player, maxStones: Int, move: Move)

  def moveGen(maxStones: Int): Gen[Move] = Gen.choose(1, maxStones)

  val written: (Player, Int) => TestContext[Move] = (player, maxStones) =>
    for {
      move <- WriterT.liftF[Gen, List[Record], Move](moveGen(maxStones))
      _ <- WriterT.tell[Gen, List[Record]](
        List(Record(player, maxStones, move))
      )
    } yield move

  case class RecordedGame(
      winner: Winner,
      records: List[Record],
      numberOfStones: Int
  )

  val recordedGameGen: Gen[RecordedGame] = for {
    numberOfStones <- Gen.choose(1, 100)
    (records, winner) <- play[TestContext](written, numberOfStones).run
  } yield RecordedGame(winner, records, numberOfStones)

  val recordedPlayers: Gen[List[Player]] =
    recordedGameGen.map(_.records.map(_.player))

  property(
    "play should ask player one then player two and continue in that fashion"
  ) = forAllNoShrink(recordedPlayers) { players =>
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

  property(
    "play should give players a maximum of between 1 and 3 stones inclusive"
  ) = forAllNoShrink(maxStonesRecorded) { max =>
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
    val numberOfStonesLeft =
      recordedGame.numberOfStones - numberOfStonesTakenThusFar
    (thisMove.maxStones, numberOfStonesLeft)
  }

  property(
    "play should give players a maximum that is no more than the stones left"
  ) = forAllNoShrink(maxStonesAndStonesLeft) {
    case (maxStones, stonesLeft) => maxStones <= stonesLeft
  }

  val winnerAndLastMove: Gen[(Winner, Player)] =
    recordedGameGen.map(game => (game.winner, game.records.last.player))
  val otherPlayer: Player => Player = {
    case PlayerOne => PlayerTwo
    case PlayerTwo => PlayerOne
  }

  property(
    "play returns the last player to move as the player"
  ) = forAllNoShrink(winnerAndLastMove) {
    case (winner, lastToMove) => winner == lastToMove
  }

  val updateWritten = (player: Player, state: Int) =>
    for {
      move <-
        WriterT.liftF[Gen, List[UpdateAndPlay], Move](moveGen(min(3, state)))
      _ <- WriterT.tell[Gen, List[UpdateAndPlay]](
        List(UpdateAndPlay(state, move))
      )
    } yield move

  type NumberOfStartingStones = Int

  case class UpdateAndPlay(update: Int, play: Int)
  type NewTestContext[A] = WriterT[Gen, List[UpdateAndPlay], A]

  val statefulGen: Gen[(NumberOfStartingStones, List[UpdateAndPlay])] = for {
    stones <- Gen.choose(1, 100)
    updatesAndPlays <-
      playWithUpdates[NewTestContext](updateWritten, stones).written
  } yield (stones, updatesAndPlays)

  val expectedTotalAndGiven: Gen[(Int, Int)] = for {
    (numberOfStones, updatesAndPlays) <- statefulGen
    index <- Gen.choose(1, updatesAndPlays.length)
  } yield {
    val (previous, thisMove :: _) = updatesAndPlays.splitAt(index - 1)
    val totalTaken = previous.map(_.play).sum
    val expected = numberOfStones - totalTaken
    val given = thisMove.update
    (expected, given)
  }

  property(
    "playWithUpdates informs the player of the game state after every move"
  ) = forAllNoShrink(expectedTotalAndGiven) {
    case (expectedTotal, givenTotal) => expectedTotal == givenTotal
  }

  property("parseStart given positive numbers should return the number") =
    forAll(Gen.posNum[Int]) { i =>
      Nim.parseStart(i.toString) == Some(i)
    }
  val nonPosGen: Gen[Int] = arbitrary[Int] suchThat (_ <= 0)
  property("parseStart given non positive integers should return None") =
    forAll(nonPosGen) { i =>
      Nim.parseStart(i.toString) == None
    }
  val maxGen: Gen[Int] = Gen.choose(1, 3)

  val maxAndTooHigh: Gen[(Int, String)] = for {
    max <- maxGen
    i <- Gen.posNum[Int] suchThat (_ > max)
  } yield (max, i.toString)

  val maxAndTooLow: Gen[(Int, String)] = for {
    max <- maxGen
    i <- nonPosGen
  } yield (max, i.toString)

  property("parseInput given ints that exceed max should return None") =
    forAll(maxAndTooHigh) {
      case (max, s) => Nim.parseInput(s, max) == None
    }

  property("parseInput given ints that exceed max should return None") =
    forAll(maxAndTooHigh) {
      case (max, s) => Nim.parseInput(s, max) == None
    }

  property("parseInput given ints that are not positive should return None") =
    forAll(maxAndTooLow) {
      case (max, s) => Nim.parseInput(s, max) == None
    }
  val maxAndOk: Gen[(Int, Int)] = for {
    max <- maxGen
    i <- Gen.choose(1, max)
  } yield (max, i)

  property(
    "parseInput given positive integers that do not exceed the max should return them"
  ) = forAll(maxAndOk) {
    case (max, i) => Nim.parseInput(i.toString, max) == Some(i)
  }

  def writtenList[A: Arbitrary]: WriterT[Gen, List[Option[A]], Option[A]] =
    writtenOpt(List(_))
  def writtenOpt[A: Arbitrary, B: Monoid](
      f: Option[A] => B
  ): WriterT[Gen, B, Option[A]] =
    for {
      o <- WriterT.liftF[Gen, B, Option[A]](Gen.option(arbitrary))
      _ <- WriterT.tell[Gen, B](f(o))
    } yield o

  def retried[A: Arbitrary]: Gen[(List[Option[A]], A)] =
    retry[WriterT[Gen, List[Option[A]], ?], A](writtenList).run

  property("retry should return the first Some") = forAll(retried[Int]) {
    case (responses, value) =>
      responses.collectFirst {
        case Some(a) => a
      } == Some(value)
  }

  sealed trait Request
  case object Normal extends Request
  case object Error extends Request

  def writtenNormal[A: Arbitrary]: WriterT[Gen, List[Request], Option[A]] =
    writtenOpt(_ => List(Normal))
  def writtenError[A: Arbitrary]: WriterT[Gen, List[Request], Option[A]] =
    writtenOpt(_ => List(Error))

  def requests[A: Arbitrary]: Gen[List[Request]] =
    retryWithError[WriterT[Gen, List[Request], ?], Option[A]](
      writtenNormal,
      writtenError
    ).written

  property(
    "retryWithError should request without error then request with error for every retry"
  ) = forAll(requests[Int]) {
    case first :: rest => first == Normal && rest.forall(_ == Error)
    case _             => false
  }

}
