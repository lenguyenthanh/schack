package se.thanh.chess.core

enum Color:
  case White, Black;

  def negate: Color =
    this match
      case White => Black
      case Black => White
