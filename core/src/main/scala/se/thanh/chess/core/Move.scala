package se.thanh.chess.core

// Todo should capture be only Boolean? what is the benefit of Role?
enum Move(from: Square, to: Square):
  case Normal(from: Square, to: Square, role: Role, capture: Boolean) extends Move(from, to)
  case Castle(from: Square, to: Square) extends Move(from, to)
  case EnPassant(from: Square, to: Square) extends Move(from, to)
  case Promotion(from: Square, to: Square, promoted: Role, capture: Boolean) extends Move(from, to)

  def uci: String =
    this match
      case Promotion(from, to, role, _) => s"${from.uci}${to.uci}${role.symbol}"
      case Castle(from, to) => {
        val k = (if to < from then Square.c1 else Square.g1).combine(from)
        s"${from.uci}${k.uci}"
      }
      case _ => s"${from.uci}${to.uci}"
