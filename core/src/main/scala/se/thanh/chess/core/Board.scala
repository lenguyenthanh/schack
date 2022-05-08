package se.thanh.chess.core

trait Board:
  def piece(square: Square): Option[Piece]
