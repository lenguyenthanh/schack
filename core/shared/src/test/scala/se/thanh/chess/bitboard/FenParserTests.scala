package se.thanh.chess.bitboard

import se.thanh.chess.core.{ Color, Square }

import munit.FunSuite

class FenParserTests extends FunSuite:
  test("parserBoard empty") {
    val result = Fen.parseBoard("8/8/8/8/8/8/8/8")
    assertEquals(result, Right(Board.empty))
  }

  test("parserBoard standard starting position") {
    val result = Fen.parseBoard("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
    assertEquals(result, Right(Board.standard))
  }

  test("standard starting position") {
    val result   = Fen.parse("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    val expected = Fen(Board.standard, State(Color.White, None, Bitboard.corners, 0, 1))
    assertEquals(result, Right(expected))
  }

  test("standard starting position 1.e4") {
    val result   = Fen.parse("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
    val board    = Board.standard.copy(pawns = 0xff00001000ef00L, occupied = 0xffff00001000efffL, white = 0x1000efffL)
    val expected = Fen(board, State(Color.Black, Square(20), Bitboard.corners, 0, 1))
    assertEquals(result, Right(expected))
  }

  test("random position") {
    val result = Fen.parse("rnbqk2r/ppp1b1pp/3p1n2/4pp2/1P6/4PN2/PBPPBPPP/RN1Q1RK1 b kq - 5 6")
    val board = Board(
      pawns = 0xc708300210ed00L,
      knights = 0x200200000200002L,
      bishops = 0x410000000001200L,
      rooks = 0x8100000000000021L,
      queens = 0x800000000000008L,
      kings = 0x1000000000000040L,
      white = 0x230ff6bL,
      black = 0x9fd7283000000000L,
      occupied = 0x9fd728300230ff6bL
    )
    val expected = Fen(board, State(Color.Black, None, 0x8100000000000000L, 5, 6))
    assertEquals(result, Right(expected))
  }
