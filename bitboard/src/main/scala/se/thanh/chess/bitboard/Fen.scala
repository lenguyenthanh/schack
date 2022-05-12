package se.thanh.chess.bitboard

import se.thanh.chess.core.Color
import se.thanh.chess.core.Square
import se.thanh.chess.core.File
import se.thanh.chess.core.Rank
import se.thanh.chess.core.Piece
import scala.collection.mutable.ListBuffer
import se.thanh.chess.core.Role

import Bitboard.*

import cats.syntax.all.*

case class Fen(board: Board, state: State):
    def us: Bitboard = board.byColor(state.turn)
    def them: Bitboard = board.byColor(!state.turn)
    def ourKing = board.king(state.turn)
    def checkers = ourKing.map(k => board.attacksTo(k, !state.turn))
    def sliderBlockers = board.sliderBlockers(state.turn)
    def isWhiteTurn = state.turn.isWhite
    def occupied = board.occupied
    def isOccupied = board.isOccupied

enum ParseFenError:
  case InvalidFenFormat
  case InvalidBoard
  case InvalidColor
  case InvalidTurn
  case InvalidEpSquare
  case InvalidCastling
  case InvalidHalfMoveClock
  case InvalidFullMoveClock

/** Fen parser temporary put it here for testing purpose
  */
object Fen:

  val standard = Fen(Board.standard, State.start)

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
    s match
      case "-" => Right(0L)
      case _ => s.toList
        .traverse(charToSquare)
        .map(ls => ls.foldRight(0L)((s, b) => (1L << s) | b))
        .toRight(ParseFenError.InvalidCastling)

  def parseEpPassantSquare(s: String): Either[ParseFenError, Option[Square]] =
    val epSquares = for
      f <- 'a' to 'h'
      r <- List('3', '6')
    yield s"$f$r"
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
