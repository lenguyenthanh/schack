package se.thanh.chess.bitboard

import se.thanh.chess.core.Square
import se.thanh.chess.core.Rank
import se.thanh.chess.core.Color
import Bitboard.*
import se.thanh.chess.core.Role
import se.thanh.chess.core.Piece

import cats.syntax.all.*
import se.thanh.chess.core.Move

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
    if (pawns.contains(s)) then Some(Role.Pawn)
    else if (knights.contains(s)) then Some(Role.Knight)
    else if (bishops.contains(s)) then Some(Role.Bishop)
    else if (rooks.contains(s)) then Some(Role.Rook)
    else if (queens.contains(s)) then Some(Role.Queen)
    else if (kings.contains(s)) then Some(Role.King)
    else None

  def colorAt(s: Square): Option[Color] =
    if (white.contains(s)) then Some(Color.White)
    else if (black.contains(s)) then Some(Color.Black)
    else None

  def pieceAt(s: Square): Option[Piece] =
    (roleAt(s), colorAt(s)).mapN(Piece.apply)

  // TODO returns Option[Boolean]
  // None in case of no king
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

  /** Find all blockers between the king and attacking sliders First we find all snipers (all potential sliders which
    * can attack the king) Then we loop over those snipers if there is only one blockers between the king and the sniper
    * we add them into the blockers list
    *
    * This is being used when checking a move is safe for the king or not
    */
  def sliderBlockers(us: Color): Bitboard =
    val ourKing = king(us) match
      case Some(s) => s
      case None    => return 0L

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

  // TODO move: Board => Board
  // We can implement as PieceMap => PieceMap
  def play(color: Color): Move => Board =
    case Move.Normal(from, to, role, _)    => discard(from).put(to, role, color)
    case Move.Promotion(from, to, role, _) => discard(from).put(to, role, color)
    case Move.EnPassant(from, to)          => discard(from).discard(to.combine(from)).put(to, Role.Pawn, color)
    case Move.Castle(from, to) =>
      val rookTo = (if to < from then Square.d1 else Square.f1).combine(to)
      val kingTo = (if to < from then Square.c1 else Square.g1).combine(to)
      discard(from).discard(to).put(rookTo, Role.Rook, color).put(kingTo, Role.King, color)

  // todo more efficient
  def discard(s: Square): Board =
    pieceAt(s).fold(this) { p =>
      val m = s.bitboard
      copy(
        pawns = updateRole(m, Role.Pawn)(p.role),
        knights = updateRole(m, Role.Knight)(p.role),
        bishops = updateRole(m, Role.Bishop)(p.role),
        rooks = updateRole(m, Role.Rook)(p.role),
        queens = updateRole(m, Role.Queen)(p.role),
        kings = updateRole(m, Role.King)(p.role),
        white = updateColor(m, Color.White)(p.color),
        black = updateColor(m, Color.Black)(p.color),
        occupied ^ m
      )
    }

  def updateRole(mask: Bitboard, role: Role): Role => Bitboard =
    case Role.Pawn if role == Role.Pawn     => pawns ^ mask
    case Role.Knight if role == Role.Knight => knights ^ mask
    case Role.Bishop if role == Role.Bishop => bishops ^ mask
    case Role.Rook if role == Role.Rook     => rooks ^ mask
    case Role.Queen if role == Role.Queen   => queens ^ mask
    case Role.King if role == Role.King     => kings ^ mask
    case _                                  => roles(role)

  def roles: Role => Bitboard =
    case Role.Pawn   => pawns
    case Role.Knight => knights
    case Role.Bishop => bishops
    case Role.Rook   => rooks
    case Role.Queen  => queens
    case Role.King   => kings

  def updateColor(mask: Bitboard, color: Color): Color => Bitboard =
    case Color.White if color == Color.White => white ^ mask
    case Color.Black if color == Color.Black => black ^ mask
    case _                                   => colors(color)

  def colors: Color => Bitboard =
    case Color.White => white
    case Color.Black => black

  def put(s: Square, role: Role, color: Color): Board =
    val b = discard(s)
    val m = s.bitboard
    b.copy(
      pawns = b.updateRole(m, Role.Pawn)(role),
      knights = b.updateRole(m, Role.Knight)(role),
      bishops = b.updateRole(m, Role.Bishop)(role),
      rooks = b.updateRole(m, Role.Rook)(role),
      queens = b.updateRole(m, Role.Queen)(role),
      kings = b.updateRole(m, Role.King)(role),
      white = b.updateColor(m, Color.White)(color),
      black = b.updateColor(m, Color.Black)(color),
      occupied = b.occupied ^ m,
    )

  def put(s: Square, p: Piece): Board =
    put(s, p.role, p.color)

  // TODO remove unsafe get
  // we believe in the integrity of bitboard
  // tests pieceMap . fromMap = identity
  def pieceMap: Map[Square, Piece] =
    occupied.occupiedSquares.map(s => (s, pieceAt(s).get)).toMap


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

  def fromMap(pieces: Map[Square, Piece]): Board =
    var pawns    = 0L
    var knights  = 0L
    var bishops  = 0L
    var rooks    = 0L
    var queens   = 0L
    var kings    = 0L
    var white    = 0L
    var black    = 0L
    var occupied = 0L

    pieces.foreach { (s, p) =>
      val position = s.bitboard
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
