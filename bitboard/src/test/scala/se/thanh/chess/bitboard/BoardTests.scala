package se.thanh.chess.bitboard

import munit.FunSuite
import Helpers.*
import se.thanh.chess.core.Color
import se.thanh.chess.core.Square

class BoardTests extends FunSuite:

  test("sliderBlockers") {
    FenFixtures.fens.foreach { str =>
      val fen           = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val result         = fen.board.sliderBlockers(fen.state.turn)
      val king = fen.ourKing.get
      val expected = fen.cBoard.sliderBlockers(king)
      assertEquals(result, expected)
    }
  }

  test("attacksTo") {
    for
      str <- FenFixtures.fens
      fen           = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      king = fen.ourKing.get
      i <- 0 to 63
      sq = Square(i).get
      color <- List(Color.White, Color.Black)
      result = fen.board.attacksTo(sq, color)
      expected = fen.cBoard.attacksTo(sq, color.isWhite)
    yield assertEquals(result, expected)
  }

