package se.thanh.chess.bitboard

import munit.FunSuite
import Helpers.*

class FenTests extends FunSuite:

  test("checkers") {
    FenFixtures.fens.foreach { str =>
      val fen      = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val result   = fen.checkers.get
      val king     = fen.ourKing.get
      val expected = fen.cBoard.attacksTo(king, !fen.isWhiteTurn)
      assertEquals(result, expected)
    }
  }
