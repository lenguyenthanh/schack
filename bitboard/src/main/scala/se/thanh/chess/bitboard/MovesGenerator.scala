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
      // todo fix
      val king = b.ourKing.get
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
        bb.lsb.map(s => (s, bb & (bb - 1L)))
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
      var capturers = b.us() & b.pawns

      capturers.lsb.fold(List()){ from =>
        val targets = from.pawnAttacks(b.turn) & b.them() & mask
      }

      val s1: List[List[Move]] = for
        from <- capturers.occupiedSquares
        targets = from.pawnAttacks(b.turn) & b.them() & mask
        to <- targets.occupiedSquares
      yield genPawnMoves(from, to, true)

      // normal pawn moves
      var singleMoves = ~b.occupied & (if b.isWhiteTurn then ((b.white & b.pawns) << 8) else ((b.black & b.pawns) >>> 8))
      var doubleMoves = ~b.occupied & (if b.isWhiteTurn then (singleMoves << 8) else (singleMoves >>> 8))
                          & Bitboard.RANKS(if b.isWhiteTurn then 3 else 4)

      val s2: List[List[Move]] = for
        to <- singleMoves.occupiedSquares
        from = Square(to + (if b.turn.isWhite then -8 else 8)).get
      yield genPawnMoves(from, to, false)

      val s3: List[Move] = for
        to <- doubleMoves.occupiedSquares
        from = Square(to + (if b.turn.isWhite then -8 else 8)).get
      yield Move.Normal(Role.Pawn, from, to, false)

      moves.toList

    def genPawnMoves(from: Square, to: Square, capture: Boolean): List[Move] =
      if to == b.seventhRank then
        List(Role.Queen, Role.Knight, Role.Rook, Role.Bishop).map(r => Move.Promotion(from, to, r, capture))
      else
        List(Move.Normal(Role.Pawn, from, to, capture))
