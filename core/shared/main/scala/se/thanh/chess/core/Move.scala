package se.thanh.chess.core

// TODO should capture be only Boolean? what is the benefit of Role?
// case Capture(from: Square, to: Square, role: Role, capture: Role)???
enum Move(val from: Square, val to: Square, val role: Role):
  case Normal(override val from: Square, override val to: Square, override val role: Role, capture: Boolean)
      extends Move(from, to, role)
  case Castle(override val from: Square, override val to: Square)    extends Move(from, to, Role.King)
  case EnPassant(override val from: Square, override val to: Square) extends Move(from, to, Role.Pawn)
  case Promotion(override val from: Square, override val to: Square, promoted: Role, capture: Boolean)
      extends Move(from, to, Role.Pawn)

  def isCapture: Boolean =
    this match
      case EnPassant(_, _)             => true
      case Castle(_, _)                => false
      case Promotion(_, _, _, capture) => capture
      case Normal(_, _, _, capture)    => capture

  def isHalfMove: Boolean =
    role != Role.Pawn && !isCapture

  def uci: String =
    this match
      case Promotion(from, to, role, _) => s"${from.uci}${to.uci}${role.symbol}"
      case Castle(from, to) => {
        val k = (if to < from then Square.c1 else Square.g1).combine(from)
        s"${from.uci}${k.uci}"
      }
      case _ => s"${from.uci}${to.uci}"
