package se.thanh.chess.core

case class Piece(role: Role, color: Color)

object Piece:
  def fromChar(ch: Char): Option[Piece] =
    ch match
      case 'p' => Some(Piece(Role.Pawn, Color.Black))
      case 'n' => Some(Piece(Role.Knight, Color.Black))
      case 'b' => Some(Piece(Role.Bishop, Color.Black))
      case 'r' => Some(Piece(Role.Rook, Color.Black))
      case 'q' => Some(Piece(Role.Queen, Color.Black))
      case 'k' => Some(Piece(Role.King, Color.Black))
      case 'P' => Some(Piece(Role.Pawn, Color.White))
      case 'N' => Some(Piece(Role.Knight, Color.White))
      case 'B' => Some(Piece(Role.Bishop, Color.White))
      case 'R' => Some(Piece(Role.Rook, Color.White))
      case 'Q' => Some(Piece(Role.Queen, Color.White))
      case 'K' => Some(Piece(Role.King, Color.White))
      case _   => None
