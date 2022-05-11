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
  extension (f: Fen)
    // todo this function should return Either[InvalidPosition, List[Move]]
    def generate: List[Move] =
      val enPassantMoves = f.state.epSquare.fold(List())(genEnPassant)
      // todo fix
      val king = f.ourKing.get
      val checkers = f.checkers.get
      val moves = if checkers == 0 then
        val targets = ~f.us()
        genNonKing(targets) ++ genSafeKing(king, targets) ++ genCastling(king)
      else genEvasions(king, checkers)
      val blockers = f.sliderBlockers
      // if (blockers != 0 || this.epSquare != 0) {
      //   moves.retain(m -> isSafe(king, m, blockers));
      // }
      List()

    def genEnPassant(ep: Square): List[Move] =
      val pawns = f.us() & f.board.pawns & ep.pawnAttacks(!f.state.turn)
      val ff: Bitboard => Option[(Square, Bitboard)] = bb =>
        bb.lsb.map(s => (s, bb & (bb - 1L)))
      List.unfold(pawns)(ff).map(s => Move.EnPassant(s, ep))


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
      var capturers = f.us() & f.board.pawns

      val s1: List[List[Move]] = for
        from <- capturers.occupiedSquares
        targets = from.pawnAttacks(f.state.turn) & f.them() & mask
        to <- targets.occupiedSquares
      yield genPawnMoves(from, to, true)

      // normal pawn moves
      val singleMoves = ~f.board.occupied & (if f.isWhiteTurn then ((f.board.white & f.board.pawns) << 8) else ((f.board.black & f.board.pawns) >>> 8))
      val doubleMoves = ~f.board.occupied & (if f.isWhiteTurn then (singleMoves << 8) else (singleMoves >>> 8))
                          & Bitboard.RANKS(if f.isWhiteTurn then 3 else 4)

      val s2: List[List[Move]] = for
        to <- singleMoves.occupiedSquares
        from = Square(to + (if f.isWhiteTurn then -8 else 8)).get
      yield genPawnMoves(from, to, false)

      val s3: List[Move] = for
        to <- doubleMoves.occupiedSquares
        from = Square(to + (if f.isWhiteTurn then -8 else 8)).get
      yield Move.Normal(Role.Pawn, from, to, false)

      moves.toList

    def genPawnMoves(from: Square, to: Square, capture: Boolean): List[Move] =
      if from == f.board.seventhRank(f.state.turn) then
        List(Role.Queen, Role.Knight, Role.Rook, Role.Bishop).map(r => Move.Promotion(from, to, r, capture))
      else
        List(Move.Normal(Role.Pawn, from, to, capture))
