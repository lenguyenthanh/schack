package se.thanh.chess.bitboard

import se.thanh.chess.core.Square
import se.thanh.chess.core.Rank
import se.thanh.chess.core.Color
import Bitboard.*
import se.thanh.chess.core.Role
import se.thanh.chess.core.Piece

import cats.syntax.all.*

case class BBoard(
    pawns: Bitboard,
    knights: Bitboard,
    rooks: Bitboard,
    bishops: Bitboard,
    queens: Bitboard,
    kings: Bitboard,
    white: Bitboard,
    black: Bitboard,
    occupied: Bitboard,

    // game states
    // color
    turn: Color,
    // possible en-passant square
    epSquare: Option[Square],
    // ???
    castlingRights: Bitboard,
):

    // ZobristHash
    def incrementalHash: Int = ???

    def isOccupied(s: Square): Boolean = occupied.contains(s)

    def us(): Bitboard = byColor(turn)
    def them(): Bitboard = byColor(!turn)

    def roleAt(s: Square): Option[Role] =
      if(pawns.contains(s)) then Some(Role.Pawn)
      else if(knights.contains(s)) then Some(Role.Knight)
      else if(bishops.contains(s)) then Some(Role.Bishop)
      else if(rooks.contains(s)) then Some(Role.Rook)
      else if(queens.contains(s)) then Some(Role.Queen)
      else if(kings.contains(s)) then Some(Role.King)
      else None

    def colorAt(s: Square): Option[Color] =
      if(white.contains(s)) then Some(Color.White)
      else if(black.contains(s)) then Some(Color.Black)
      else None

    def pieceAt(s: Square): Option[Piece] =
      (roleAt(s), colorAt(s)).mapN(Piece.apply)

    def whiteAt(s: Square): Boolean =
      colorAt(s).fold(false)(c => c == Color.White)

    def blackAt(s: Square): Boolean =
      colorAt(s).fold(false)(c => c == Color.Black)

    def king(color: Color): Square =
      (kings & byColor(color)).lsb

    def ourKing: Square = king(turn)

    // TODO is this useful?
    def roleByColor(role: Bitboard, color: Color): Bitboard =
      color match
        case Color.White => role & white
        case Color.Black => role & black

    def isWhiteTurn = turn.isWhite
    def isBlackTurn = !isWhiteTurn

    def attacksTo(s: Square, attacker: Color): Bitboard =
      byColor(attacker) & (
        s.rookAttacks(occupied) & (rooks ^ queens) |
        s.bishopAttacks(occupied) & (bishops ^ queens) |
        s.knightAttacks & kings |
        s.pawnAttacks(!attacker) & pawns
      )

    def isCheck() =
      attacksTo(king(turn), !turn) != 0

    def seventhRank: Rank = turn match
                        case Color.White => Rank.seven
                        case Color.Black => Rank.two
    /** Find all blockers between the king an sliders
      * First we find all snipers (all potential sliders which can attack the king)
      * Then we loop over those snipers if there is no only one blockers between
      * the king and the sniper we add them into the blockers list
      */
    def sliderBlockers(king: Square): Bitboard =
      var snipers = them() & (
          king.rookAttacks(0L) & (rooks ^ queens) |
          king.bishopAttacks(0L) & (bishops ^ queens)
        )
      var blockers = 0L
      while
        snipers != 0
      do
        val sniper = snipers.lsb
        val between = Bitboard.between(king, sniper) & occupied
        if(!between.moreThanOne) blockers |= between
        snipers &= snipers - 1L
      blockers

    private def byColor: Color => Bitboard =
        case Color.White => white
        case Color.Black => black

object BBoard:

  // start of standard position
  val STANDARD = BBoard(
    pawns = 0xff00000000ff00L,
    knights = 0x4200000000000042L,
    bishops = 0x2400000000000024L,
    rooks = 0x8100000000000081L,
    queens = 0x800000000000008L,
    kings = 0x1000000000000010L,
    white = 0xffffL,
    black = 0xffff000000000000L,
    occupied = 0xffff00000000ffffL,
    turn = Color.White,
    epSquare = None,
    castlingRights = 0x8100000000000081L,
  )

  // empty board for testing purpose only
  val EMPTY = BBoard(
    pawns = 0L,
    knights = 0L,
    bishops = 0L,
    rooks = 0L,
    queens = 0L,
    kings = 0L,
    white = 0L,
    black = 0L,
    occupied = 0L,
    turn = Color.White,
    epSquare = None,
    castlingRights = 0L,
  )

