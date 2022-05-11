package se.thanh.chess.core

enum Role:
  case Pawn
  case Knight
  case Bishop
  case Rook
  case Queen
  case King

  def symbol: Char =
    this match
      case Pawn => 'p'
      case Knight => 'n'
      case Bishop => 'b'
      case Rook => 'r'
      case Queen => 'q'
      case King => 'k'
