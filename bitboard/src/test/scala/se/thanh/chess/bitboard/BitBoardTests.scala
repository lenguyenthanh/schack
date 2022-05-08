package se.thanh.chess.bitboard

import munit.ScalaCheckSuite
import org.lichess.compression.game.Bitboard as CBB
import se.thanh.chess.core.Square
import Bitboard.*
import org.scalacheck.Prop
import se.thanh.chess.core.Arbitraries.given
import se.thanh.chess.core.Color

class BitboardTests extends ScalaCheckSuite:

  val allDeltas =
    List(KING_DELTAS, KNIGHT_DELTAS, BISHOP_DELTAS, ROOK_DELTAS, KING_DELTAS, WHITE_PAWN_DELTAS, BLACK_PAWN_DELTAS)

  def as(array: Array[Int]) = array.map(i => s"$i, ").mkString

  test("single cases") {
    val occupied = 20292374
    val deltas   = KING_DELTAS
    val s        = Square(43).get
    val result   = Bitboard.slidingAttacks(s, occupied, deltas)
    val expected = CBB.slidingAttacks(s, occupied, deltas)
    assertEquals(result, expected)
  }

  test("slidingAttack with all occupied") {
    val occupied = Bitboard.ALL
    for
      i      <- 0 to 63
      deltas <- allDeltas
      s        = Square(i).get
      result   = Bitboard.slidingAttacks(s, occupied, deltas)
      expected = CBB.slidingAttacks(s, occupied, deltas)
    yield assertEquals(result, expected)
  }

  property("slidingAttack check") {
    Prop.forAll { (occupied: Long, s: Square) =>
      val result = for
        deltas <- allDeltas
        // _        = println(s"o: $occupied, s: $s, d: ${as(deltas)}")
        result   = Bitboard.slidingAttacks(s, occupied, deltas)
        expected = CBB.slidingAttacks(s, occupied, deltas)
      yield result == expected
      result.fold(true)((a, b) => a && b)
    }
  }

  test("init function") {
    assertEquals(Bitboard.KNIGHT_ATTACKS.toSeq, CBB.KNIGHT_ATTACKS.toSeq)
    assertEquals(Bitboard.WHITE_PAWN_ATTACKS.toSeq, CBB.WHITE_PAWN_ATTACKS.toSeq)
    assertEquals(Bitboard.BLACK_PAWN_ATTACKS.toSeq, CBB.BLACK_PAWN_ATTACKS.toSeq)
    assertEquals(Bitboard.ATTACKS.toSeq, CBB.ATTACKS.toSeq)
    (0 until 64).foreach { i =>
      assertEquals(Bitboard.RAYS(i).toSeq, CBB.RAYS(i).toSeq)
      assertEquals(Bitboard.BETWEEN(i).toSeq, CBB.BETWEEN(i).toSeq)
    }
  }

  property("bitshop attacks") {
    Prop.forAll { (occupied: Long, s: Square) =>
      s.bishopAttacks(occupied) == CBB.bishopAttacks(s, occupied)
    }
  }

  property("rook attacks") {
    Prop.forAll { (occupied: Long, s: Square) =>
      s.rookAttacks(occupied) == CBB.rookAttacks(s, occupied)
    }
  }

  property("queen attacks") {
    Prop.forAll { (occupied: Long, s: Square) =>
      s.queenAttacks(occupied) == CBB.queenAttacks(s, occupied)
    }
  }

  property("pawn attacks") {
    Prop.forAll { (s: Square) =>
      s.pawnAttacks(Color.White) == CBB.pawnAttacks(true, s)
      s.pawnAttacks(Color.Black) == CBB.pawnAttacks(false, s)
    }
  }
