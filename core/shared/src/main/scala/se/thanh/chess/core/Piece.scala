package se.thanh.chess.core

import Role.*
import Color.*

case class Piece(role: Role, color: Color)

object Piece:
  val all: Map[Char, Piece] = Map(
    'P' -> Piece(Pawn, White),
    'N' -> Piece(Knight, White),
    'B' -> Piece(Bishop, White),
    'R' -> Piece(Rook, White),
    'Q' -> Piece(Queen, White),
    'K' -> Piece(King, White),
    'p' -> Piece(Pawn, Black),
    'n' -> Piece(Knight, Black),
    'b' -> Piece(Bishop, Black),
    'r' -> Piece(Rook, Black),
    'q' -> Piece(Queen, Black),
    'k' -> Piece(King, Black)
  )

  def fromChar(ch: Char): Option[Piece] = all.get(ch)
