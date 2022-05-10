package se.thanh.chess.bitboard

import se.thanh.chess.core.Color
import se.thanh.chess.core.Square
import se.thanh.chess.core.File
import se.thanh.chess.core.Rank
import se.thanh.chess.core.Piece
import scala.collection.mutable.ListBuffer
import se.thanh.chess.core.Role

import cats.syntax.all.*

/** Fen parser temporary put it here for testing purpose
  */

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
)

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

case class State(
    // color
    turn: Color,
    // possible en-passant square
    epSquare: Option[Square],
    // 1 in position of a rook means castling right
    // maybe we don't need a long for this
    castlingRights: Bitboard,
    // The halfmove clock specifies a decimal number of half moves with respect
    // to the 50 move draw rule. It is reset to zero after a capture or a pawn
    // move and incremented otherwise.
    halftMoves: Int,
    // The number of the full moves in a game. It starts at 1,
    // and is incremented after each Black's move.
    fullMoves: Int
)

case class Fen(board: Board, state: State)

enum ParseFenError:
  case InvalidFenFormat
  case InvalidBoard
  case InvalidColor
  case InvalidTurn
  case InvalidEpSquare
  case InvalidCastling
  case InvalidHalfMoveClock
  case InvalidFullMoveClock

object Fen:

  def parse(fen: String): Either[ParseFenError, Fen] =
    val parts = fen.split(' ').filter(s => !s.isEmpty)
    if parts.size != 6 then Left(ParseFenError.InvalidFenFormat)
    else
      for
        board          <- parseBoard(parts(0))
        turn           <- parseColor(parts(1))
        castlingRights <- parseCastlingRights(parts(2))
        epSquare       <- parseEpPassantSquare(parts(3))
        halftMoves     <- parts(4).toIntOption.toRight(ParseFenError.InvalidHalfMoveClock)
        fullMoves      <- parts(5).toIntOption.toRight(ParseFenError.InvalidHalfMoveClock)
      yield Fen(board, State(turn, epSquare, castlingRights, halftMoves, fullMoves))

  def parseColor(s: String): Either[ParseFenError, Color] =
    s match
      case "w" => Right(Color.White)
      case "b" => Right(Color.Black)
      case _   => Left(ParseFenError.InvalidColor)

  def parseCastlingRights(s: String): Either[ParseFenError, Bitboard] =
    s.toList
      .traverse(charToSquare)
      .map(ls => ls.foldRight(0L)((s, b) => (1L << s) | b))
      .toRight(ParseFenError.InvalidCastling)

  def parseEpPassantSquare(s: String): Either[ParseFenError, Option[Square]] =
    val epSquares = for
      f <- 'a' to 'h'
      r <- List('3', '6')
    yield List(f, r).mkString
    s match
      case ep if epSquares contains ep => Right(Square.fromString(ep))
      case "-"                         => Right(None)
      case _                           => Left(ParseFenError.InvalidEpSquare)

  def charToSquare: (c: Char) => Option[Square] =
    case 'k' => Some(Square.square(File.h, Rank.eighth))
    case 'q' => Some(Square.square(File.a, Rank.eighth))
    case 'K' => Some(Square.square(File.h, Rank.first))
    case 'Q' => Some(Square.square(File.a, Rank.first))
    case _   => None

  def parseBoard(boardFen: String): Either[ParseFenError, Board] =
    var rank   = 7
    var file   = 0
    val iter   = boardFen.iterator
    val pieces = ListBuffer[(Piece, Square)]()
    while iter.hasNext
    do
      iter.next match
        case '/' if file == 8 => {
          file = 0
          rank -= 1
          if rank < 0 then return Left(ParseFenError.InvalidBoard)
        }
        case ch if ('1' to '8' contains ch) => {
          // println(s"do $ch ${ch - '0'}")
          file += (ch - '0')
          if file > 8 then return Left(ParseFenError.InvalidBoard)
        }
        case ch => {
          // println(s"do $ch $file $rank")
          (pieceFromChar(ch), File(file), Rank(rank))
            .mapN((p, f, r) => (p, Square.square(f, r)))
            .match
              case Some(p) => pieces.addOne(p)
              case None    => return Left(ParseFenError.InvalidBoard)
          file += 1
        }
    // println(pieces)
    Right(Board.fromPieces(pieces.toList))

  def pieceFromChar(ch: Char): Option[Piece] =
    ch match
      case 'p' => Some(Piece(Role.Pawn, Color.Black))
      case 'n' => Some(Piece(Role.Knight, Color.Black))
      case 'b' => Some(Piece(Role.Bishop, Color.Black))
      case 'r' => Some(Piece(Role.Rook, Color.Black))
      case 'q' => Some(Piece(Role.Queen, Color.Black))
      case 'k' => Some(Piece(Role.King, Color.Black))
      case 'P' => Some(Piece(Role.Pawn, Color.White))
      case 'N' => Some(Piece(Role.Knight, Color.White))
      case 'B' => Some(Piece(Role.Bishop, Color.White))
      case 'R' => Some(Piece(Role.Rook, Color.White))
      case 'Q' => Some(Piece(Role.Queen, Color.White))
      case 'K' => Some(Piece(Role.King, Color.White))
      case _   => None
