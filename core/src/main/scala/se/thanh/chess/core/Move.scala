package se.thanh.chess.core

// Todo should capture be only Boolean? what is the benefit of Role?
enum Move:
  case Normal(role: Role, from: Square, to: Square, capture: Boolean)
  case CastleKingSide
  case CastleQueenSide
  case EnPassant(from: Square, to: Square) // always a capture
  case Promotion(from: Square, to: Square, promoted: Role, capture: Boolean)
