package se.thanh.chess.bitboard

import munit.FunSuite
import Helpers.*
import se.thanh.chess.core.Color
import se.thanh.chess.core.Square

import cats.syntax.all.*
import se.thanh.chess.core.Piece

class BoardTests extends FunSuite:

  test("sliderBlockers") {
    FenFixtures.fens.foreach { str =>
      val fen      = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val result   = fen.board.sliderBlockers(fen.state.turn)
      val king     = fen.ourKing.get
      val expected = fen.cBoard.sliderBlockers(king)
      assertEquals(result, expected)
    }
  }

  test("attacksTo") {
    for
      str <- FenFixtures.fens
      fen  = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      king = fen.ourKing.get
      i <- 0 to 63
      sq = Square(i).get
      color <- List(Color.White, Color.Black)
      result   = fen.board.attacksTo(sq, color)
      expected = fen.cBoard.attacksTo(sq, color.isWhite)
    yield assertEquals(result, expected)
  }

  test("discard with standard board") {
    val squaresToDiscards = List.range(0, 16) ++ List.range(48, 64)
    val result            = squaresToDiscards.foldRight(Board.standard)((s, b) => b.discard(Square(s).get))
    assertEquals(result, Board.empty)
  }

  test("discard with test fixtures") {
    FenFixtures.fens.foreach { str =>
      val fen    = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val result = List.range(0, 64).foldRight(fen.board)((s, b) => b.discard(Square(s).get))
      assertEquals(result, Board.empty)
    }
  }

  test("put with standard board") {
      val ss     = List.range(0, 64).map(Square(_).get)
      val board = Board.standard
      val pieces = ss.mapFilter(s => board.pieceAt(s).map((s, _)))
      val result = pieces.foldRight(Board.empty)((s, b) => b.put(s._1, s._2))
      assertEquals(result, board)
  }

  test("put with test fixtures") {
    FenFixtures.fens.foreach { str =>
      val fen    = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val ss     = List.range(0, 64).map(Square(_).get)
      val pieces: List[(Square, Piece)] = ss.mapFilter(s => fen.board.pieceAt(s).map((s, _)))
      val result = pieces.foldRight(Board.empty)((s, b) => b.put(s._1, s._2))
      assertEquals(result, fen.board)
    }
  }
