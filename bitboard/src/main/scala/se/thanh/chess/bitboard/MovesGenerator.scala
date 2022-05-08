package se.thanh.chess.bitboard

import se.thanh.chess.core.Variant
import se.thanh.chess.core.Move

object StandardMovesGenerator:
    extension (b: BBoard)
      def generate: List[Move] = ???

      def genEnPassant(): List[Move] =
        var pawns = b.us() & b.pawns
        ???

