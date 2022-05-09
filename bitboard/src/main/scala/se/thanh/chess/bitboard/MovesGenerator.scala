package se.thanh.chess.bitboard

import se.thanh.chess.core.Variant
import se.thanh.chess.core.Move
import se.thanh.chess.core.Square
import se.thanh.chess.core.Color
import se.thanh.chess.bitboard.Bitboard.*

/** Idea is each variant can have its own movs generator Now I just want to finish standard variant first
  */
object StandardMovesGenerator:
  extension (b: BBoard)
    def generate: List[Move] = ???

    def genEnPassant(ep: Square): List[Move] =
      val pawns = b.us() & b.pawns & ep.pawnAttacks(!b.turn)
      val f: Bitboard => Option[(Square, Bitboard)] = bb =>
        val newPawns = bb & (bb - 1L)
        Option.when(bb != 0)((bb.lsb, newPawns))
      List.unfold(pawns)(f).map(s => Move.EnPassant(s, ep))
