package se.thanh.chess.bitboard

import se.thanh.chess.core.Square
import se.thanh.chess.core.Color
import Bitboard.*

case class BBoard(
    pawns: Bitboard,
    knights: Bitboard,
    rooks: Bitboard,
    bishops: Bitboard,
    queens: Bitboard,
    kings: Bitboard,
    white: Bitboard,
    black: Bitboard,
    occupied: Bitboard,

    // game states
    // color
    turn: Color,
    // possible en-passant square
    epSquare: Option[Square],
    // ???
    castlingRights: Bitboard,
):

    // ZobristHash
    def incrementalHash: Int = ???

    def isOccupied(s: Square): Boolean = occupied.contains(s)

    def us(): Bitboard = byColor(turn)
    def them(): Bitboard = byColor(turn)

    private def byColor: Color => Bitboard =
        case Color.White => white
        case Color.Black => black

object BBoard:

  // start of standard position
  val STANDARD = BBoard(
    pawns = 0xff00000000ff00L,
    knights = 0x4200000000000042L,
    bishops = 0x2400000000000024L,
    rooks = 0x8100000000000081L,
    queens = 0x800000000000008L,
    kings = 0x1000000000000010L,
    white = 0xffffL,
    black = 0xffff000000000000L,
    occupied = 0xffff00000000ffffL,
    turn = Color.White,
    epSquare = None,
    castlingRights = 0x8100000000000081L,
  )

  // empty board for testing purpose only
  val EMPTY = BBoard(
    pawns = 0L,
    knights = 0L,
    bishops = 0L,
    rooks = 0L,
    queens = 0L,
    kings = 0L,
    white = 0L,
    black = 0L,
    occupied = 0L,
    turn = Color.White,
    epSquare = None,
    castlingRights = 0L,
  )

