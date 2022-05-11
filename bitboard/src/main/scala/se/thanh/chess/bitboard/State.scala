package se.thanh.chess.bitboard

import se.thanh.chess.core.Color
import se.thanh.chess.core.Square
import se.thanh.chess.core.File
import se.thanh.chess.core.Rank
import se.thanh.chess.core.Piece
import scala.collection.mutable.ListBuffer
import se.thanh.chess.core.Role
import Bitboard.*
import cats.syntax.all.*

/** All the information needed to restart the game beside board position
  * We only deal with standard variant now, but in the future each variant
  * can and should have different state.
  */
case class State(
    // color
    turn: Color,
    // possible en-passant square
    epSquare: Option[Square],
    // 1 in position of a rook means castling right
    // maybe we don't need a long for this
    castlingRights: Bitboard,
    // The halfmove clock specifies a decimal number of half moves with respect
    // to the 50 move draw rule. It is reset to zero after a capture or a pawn
    // move and incremented otherwise.
    halftMoves: Int,
    // The number of the full moves in a game. It starts at 1,
    // and is incremented after each Black's move.
    fullMoves: Int
)