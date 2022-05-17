package se.thanh.chess.core

import scala.annotation.targetName

enum Color:
  case White, Black

  @targetName("negate")
  def unary_! =
    this match
      case White => Black
      case Black => White

  def isWhite =
    this match
      case White => true
      case Black => false

  def isBlack = !isWhite

object Color:

  def fromBoolean(isWhite: Boolean): Color =
    if isWhite then Color.White
    else Color.Black
