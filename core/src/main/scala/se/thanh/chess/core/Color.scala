package se.thanh.chess.core

import scala.annotation.targetName

enum Color:
  case White, Black

  @targetName("negate")
  def unary_! =
    this match
      case White => Black
      case Black => White
