package se.thanh.chess.bitboard

import munit.FunSuite
import StandardMovesGenerator.*
import Helpers.*
import se.thanh.chess.core.Square
import se.thanh.chess.core.File
import se.thanh.chess.core.Rank
import se.thanh.chess.core.Move
import se.thanh.chess.core.Color
import org.lichess.compression.game.MoveList

class StandardMovesGeneratorTests extends FunSuite:

  test("genEnPassant 1") {
    val b        = Board.empty.copy(pawns = 0x7000000000L, white = 0x5000000000L, black = 0x2000000000L)
    val ep       = Square.square(File.f, Rank.sixth)
    val fen      = Fen(b, State(Color.White, Some(ep), Bitboard.corners, 5, 10))
    val moves    = fen.genEnPassant(ep)
    val expected = Set(Move.EnPassant(Square(36).get, ep), Move.EnPassant(Square(38).get, ep))
    assertEquals(moves.toSet, expected.toSet)
  }

  test("genPawn for standard position") {
    val fen           = Fen.standard
    val targets       = ~fen.us
    val moves         = fen.genPawn(targets)
    val moveList      = MoveList()
    val expectedMoves = fen.cBoard.genPawn(targets, moveList)
    assertMoves(moves, moveList)
  }

  test("genPawn with fenFixtures") {
    FenFixtures.fens.foreach { str =>
      val fen           = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val targets       = ~fen.us
      val moves         = fen.genPawn(targets)
      val moveList      = MoveList()
      val expectedMoves = fen.cBoard.genPawn(targets, moveList)
      assertMoves(moves, moveList)
    }
  }

  test("genKnight with fenFixtures") {
    FenFixtures.fens.foreach { str =>
      val fen           = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val targets       = ~fen.us
      val moves         = fen.genKnight(targets)
      val moveList      = MoveList()
      val expectedMoves = fen.cBoard.genKnight(targets, moveList)
      assertMoves(moves, moveList)
    }
  }

  test("genBishop with fenFixtures") {
    FenFixtures.fens.foreach { str =>
      val fen           = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val targets       = ~fen.us
      val moves         = fen.genBishop(targets)
      val moveList      = MoveList()
      val expectedMoves = fen.cBoard.genBishop(targets, moveList)
      assertMoves(moves, moveList)
    }
  }

  test("genRook with fenFixtures") {
    FenFixtures.fens.foreach { str =>
      val fen           = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val targets       = ~fen.us
      val moves         = fen.genRook(targets)
      val moveList      = MoveList()
      val expectedMoves = fen.cBoard.genRook(targets, moveList)
      assertMoves(moves, moveList)
    }
  }

  test("genQueen with fenFixtures") {
    FenFixtures.fens.foreach { str =>
      val fen           = Fen.parse(str).getOrElse(throw RuntimeException("boooo"))
      val targets       = ~fen.us
      val moves         = fen.genQueen(targets)
      val moveList      = MoveList()
      val expectedMoves = fen.cBoard.genQueens(targets, moveList)
      assertMoves(moves, moveList)
    }
  }

  private def assertMoves(moves: List[Move], moveList: MoveList) =
    assertEquals(moves.length, moveList.size)
    println(moves.toSet)
    println(moves.map(m => m.uci).toSet)
    assertEquals(moves.map(m => m.uci).toSet, moveList.uciSet)

