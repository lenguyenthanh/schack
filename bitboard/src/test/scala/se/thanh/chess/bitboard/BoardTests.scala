package se.thanh.chess.bitboard

import munit.FunSuite
import Helpers.*
import se.thanh.chess.core.Color
import se.thanh.chess.core.Role
import se.thanh.chess.core.Move
import se.thanh.chess.core.Square
import se.thanh.chess.core.Piece

import cats.syntax.all.*

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

  test("put case 1") {
    val fen = Fen.parse("8/8/8/8/p7/1P6/8/8 w - - 0 1").getOrElse(throw RuntimeException("booo"))
    val result = fen.board.put(Square.a4, Piece(Role.Pawn, Color.White))
    val expectedMap = fen.board.pieceMap + (Square.a4 -> Piece(Role.Pawn, Color.White))
    assertEquals(result.pieceMap, expectedMap)
  }

  test("play with standard board") {
      val board = Board.standard
      val newBoard = board.play(Color.White)(Move.Normal(Square.e2, Square.e4, Role.Pawn, false))
      val expectedMap = (board.pieceMap - Square.e2) + (Square.e4 -> Piece(Role.Pawn, Color.White))
      val result = newBoard.pieceMap
      assertEquals(result, expectedMap)
  }

  test("case 1") {
    val fen = Fen.parse("rnbqkbnr/1ppppppp/8/8/p7/PPP5/3PPPPP/RNBQKBNR w KQkq - 0 4").getOrElse(throw RuntimeException("booo"))
    val result = fen.play(Move.Normal(Square.b3, Square.a4, Role.Pawn, true))
    val expectedMap = (fen.board.pieceMap - Square.b3) + (Square.a4 -> Piece(Role.Pawn, Color.White))
    assertEquals(result.board.pieceMap, expectedMap)
  }
