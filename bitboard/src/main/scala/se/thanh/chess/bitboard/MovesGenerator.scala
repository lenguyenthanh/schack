package se.thanh.chess.bitboard

import se.thanh.chess.core.Variant
import se.thanh.chess.core.Move
import se.thanh.chess.core.Square
import se.thanh.chess.core.Color
import se.thanh.chess.bitboard.Bitboard.*
import se.thanh.chess.core.Role
import scala.collection.mutable.ListBuffer

/** The idea is each variant can have its own movs generator.
  * Now I just want to finish standard variant first
  */
object StandardMovesGenerator:
  extension (b: BBoard)
    def generate: List[Move] =
      val enPassantMoves = b.epSquare.fold(List())(genEnPassant)
      val king = b.ourKing
      val checkers = b.attacksTo(king, !b.turn)
      val moves = if checkers == 0 then
        val targets = ~b.us()
        genNonKing(targets) ++ genSafeKing(king, targets) ++ genCastling(king)
      else genEvasions(king, checkers)
      val blockers = b.sliderBlockers(king)
      // if (blockers != 0 || this.epSquare != 0) {
      //   moves.retain(m -> isSafe(king, m, blockers));
      // }
      List()

    def genEnPassant(ep: Square): List[Move] =
      val pawns = b.us() & b.pawns & ep.pawnAttacks(!b.turn)
      val f: Bitboard => Option[(Square, Bitboard)] = bb =>
        val newPawns = bb & (bb - 1L)
        Option.when(bb != 0)((bb.lsb, newPawns))
      List.unfold(pawns)(f).map(s => Move.EnPassant(s, ep))


    def genNonKing(mask: Bitboard): List[Move] = ???
    def genSafeKing(king: Square, mask: Bitboard): List[Move] = ???
    def genCastling(king: Square): List[Move] = ???
    def genEvasions(king: Square, checkers: Bitboard): List[Move] = ???

    /** Generate all pawn moves except en passant
      * This includes
      *   - captures
      *   - single square moves
      *   - double square moves
      * @mask: bitboard contains empty square or enemy pieces
      * TODO @mask includes enemy king now, which should not be
      * because enemy cannot be captured by law
      */
    def genPawn(mask: Bitboard): List[Move] =
      var moves = ListBuffer[Move]()

      // pawn captures
      var captures = b.us() & b.pawns
      while
        captures != 0
      do
        val from = captures.lsb
        var targets = from.pawnAttacks(b.turn) & b.them() & mask
        while
          targets != 0
        do
          val to = targets.lsb
          moves.addAll(genPawnMoves(from, to, true))
          targets &= (targets - 1L)

      // normal pawn moves
      var singleMoves = ~b.occupied & (if b.isWhiteTurn then ((b.white & b.pawns) << 8) else ((b.black & b.pawns) >>> 8))
      var doubleMoves = ~b.occupied & (if b.isWhiteTurn then (singleMoves << 8) else (singleMoves >>> 8))
                          & Bitboard.RANKS(if b.isWhiteTurn then 3 else 4)

      while
        singleMoves != 0
      do
        val to = singleMoves.lsb
        val from = Square(to + (if b.turn.isWhite then -8 else 8)).get
        moves.addAll(genPawnMoves(from, to, false))

      while
        doubleMoves != 0
      do
        val to = doubleMoves.lsb
        val from = Square(to + (if b.turn.isWhite then -8 else 8)).get
        moves.addOne(Move.Normal(Role.Pawn, from, to, false))

      moves.toList

    def genPawnMoves(from: Square, to: Square, capture: Boolean): List[Move] =
      if to == b.seventhRank then
        List(Role.Queen, Role.Knight, Role.Rook, Role.Bishop).map(r => Move.Promotion(from, to, r, capture))
      else
        List(Move.Normal(Role.Pawn, from, to, capture))
