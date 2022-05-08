package se.thanh.chess.core

enum Move:
  case Normal(role: Role, from: Square, to: Square, capture: Option[Role])
  case CastleKingSide
  case CastleQueenSide
  case EnPassant(from: Square, to: Square)
  case Promotion(from: Square, to: Square, promoted: Role, capture: Option[Role])
