package se.thanh.chess.bitboard

import munit.FunSuite
import StandardMovesGenerator.*
import se.thanh.chess.core.Square

class StandardMovesGeneratorTests extends FunSuite:
  test("genEnPassant 1") {
    val bb = BBoard.EMPTY.copy(pawns = 0x7000000000L, white = 0x5000000000L, black = 0x2000000000L, epSquare=Square(45))
    val ep = Square(45).get
    val moves = bb.genEnPassant(ep)
    println(moves)
    assertEquals(moves.size, 2)
  }
