package se.thanh.chess.bitboard

import se.thanh.chess.core.Square
import se.thanh.chess.core.Rank
import se.thanh.chess.core.Color
import Bitboard.*
import se.thanh.chess.core.Role
import se.thanh.chess.core.Piece

import cats.syntax.all.*

case class Board(
    pawns: Bitboard,
    knights: Bitboard,
    bishops: Bitboard,
    rooks: Bitboard,
    queens: Bitboard,
    kings: Bitboard,
    white: Bitboard,
    black: Bitboard,
    occupied: Bitboard
):
  def isOccupied(s: Square): Boolean = occupied.contains(s)

  def sliders = bishops ^ rooks ^ queens

  def byColor: Color => Bitboard =
      case Color.White => white
      case Color.Black => black

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

  def king(color: Color): Option[Square] =
    (kings & byColor(color)).lsb

  def attacksTo(s: Square, attacker: Color): Bitboard =
    attacksTo(s, attacker, occupied)

  def attacksTo(s: Square, attacker: Color, occupied: Bitboard): Bitboard =
    byColor(attacker) & (
      s.rookAttacks(occupied) & (rooks ^ queens) |
      s.bishopAttacks(occupied) & (bishops ^ queens) |
      s.knightAttacks & knights |
      s.kingAttacks & kings |
      s.pawnAttacks(!attacker) & pawns
    )

  def isCheck(color: Color): Boolean =
    king(color).fold(false)(k => attacksTo(k, !color) != 0)

  /** Find all blockers between the king and attacking sliders
    * First we find all snipers (all potential sliders which can attack the king)
    * Then we loop over those snipers if there is only one blockers between
    * the king and the sniper we add them into the blockers list
    *
    * This is being used when checking a move is safe for the king or not
    */
  def sliderBlockers(us: Color): Bitboard =
    val ourKing = king(us) match
      case Some(s) => s
      case None => return 0L

    val snipers = byColor(!us) & (
        ourKing.rookAttacks(0L) & (rooks ^ queens) |
        ourKing.bishopAttacks(0L) & (bishops ^ queens)
      )

    val bs = for
      sniper <- snipers.occupiedSquares
      between = Bitboard.between(ourKing, sniper) & occupied
      if !between.moreThanOne
    yield between

    bs.fold(0L)((a, b) => a | b)

object Board:
  val empty = Board(
    pawns = 0L,
    knights = 0L,
    bishops = 0L,
    rooks = 0L,
    queens = 0L,
    kings = 0L,
    white = 0L,
    black = 0L,
    occupied = 0L
  )
  val standard = Board(
    pawns = 0xff00000000ff00L,
    knights = 0x4200000000000042L,
    bishops = 0x2400000000000024L,
    rooks = 0x8100000000000081L,
    queens = 0x800000000000008L,
    kings = 0x1000000000000010L,
    white = 0xffffL,
    black = 0xffff000000000000L,
    occupied = 0xffff00000000ffffL
  )

  def fromPieces(pieces: List[(Piece, Square)]): Board =
    var pawns    = 0L
    var knights  = 0L
    var bishops  = 0L
    var rooks    = 0L
    var queens   = 0L
    var kings    = 0L
    var white    = 0L
    var black    = 0L
    var occupied = 0L

    pieces.foreach { (p, s) =>
      val position = (1L << s)
      occupied |= position
      p.role match
        case Role.Pawn   => pawns |= position
        case Role.Knight => knights |= position
        case Role.Bishop => bishops |= position
        case Role.Rook   => rooks |= position
        case Role.Queen  => queens |= position
        case Role.King   => kings |= position

      p.color match
        case Color.White => white |= position
        case Color.Black => black |= position
    }
    Board(pawns, knights, bishops, rooks, queens, kings, white, black, occupied)
