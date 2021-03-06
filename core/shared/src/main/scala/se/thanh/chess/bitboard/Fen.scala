package se.thanh.chess.bitboard

import scala.collection.mutable.ListBuffer

import se.thanh.chess.core.*

import cats.syntax.all.*

import Bitboard.*

case class Fen(board: Board, state: State):
  def us: Bitboard   = board.byColor(state.turn)
  def them: Bitboard = board.byColor(!state.turn)
  def ourKing        = board.king(state.turn)
  def checkers       = ourKing.map(board.attacksTo(_, !state.turn))
  def sliderBlockers = board.sliderBlockers(state.turn)
  def isWhiteTurn    = state.turn.isWhite
  def occupied       = board.occupied
  def isOccupied     = board.isOccupied

  // Used for filtering candidate moves that would leave put the king in check.
  def isSafe(king: Square, move: Move, blockers: Bitboard): Boolean =
    move match
      case Move.Normal(from, to, _, _) =>
        val result = !(us & blockers).contains(from) || Bitboard.aligned(from, to, king)
        result
      case Move.EnPassant(from, to) =>
        val newOccupied = (occupied ^ from.bitboard ^ to.combine(from).bitboard) | to.bitboard
        (king.rookAttacks(newOccupied) & them & (board.rooks ^ board.queens)) == 0L &&
        (king.bishopAttacks(newOccupied) & them & (board.bishops ^ board.queens)) == 0L
      case _ => true

  // TODO now it works with valid move only
  def play(move: Move): Fen =
    Fen(playBoard(move), playState(move))

  // TODO should we validate the move here?
  // Either[InvalidMove, Board]
  def playBoard: Move => Board =
    board.play(state.turn)

  def playState(move: Move): State =
    val halfMoves = if move.isHalfMove then state.halfMoves + 1 else 0
    val fullMoves = if state.turn.isBlack then state.fullMoves + 1 else state.fullMoves
    val turn      = !state.turn
    val halfCastlingRights =
      if move.isCapture then state.castlingRights & ~move.to.bitboard
      else state.castlingRights
    val haftState = state.copy(
      turn = turn,
      halfMoves = halfMoves,
      fullMoves = fullMoves,
      epSquare = None,
      castlingRights = halfCastlingRights
    )

    move match
      case Move.Normal(from, to, Role.Pawn, _) =>
        val epSquare: Option[Square] =
          if Math.abs(from - to) == 16 then
            // TODO calculate their pawns attacks
            Square(from + (if isWhiteTurn then 8 else -8))
          else None
        haftState.copy(epSquare = epSquare)
      case Move.Normal(from, _, Role.Rook, _) =>
        val castlingRights = halfCastlingRights & ~from.bitboard
        haftState.copy(castlingRights = castlingRights)
      case Move.Normal(_, _, Role.King, _) | Move.Castle(_, _) =>
        val castlingRights = halfCastlingRights & Bitboard.RANKS(state.turn.lastRank)
        haftState.copy(castlingRights = castlingRights)
      case _ => haftState

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
      case _ =>
        s.toList
          .traverse(charToSquare)
          .map(_.foldRight(0L)((s, b) => s.bitboard | b))
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
    val pieces = ListBuffer[(Square, Piece)]()
    while iter.hasNext
    do
      iter.next match
        case '/' if file == 8 => {
          file = 0
          rank -= 1
          if rank < 0 then return Left(ParseFenError.InvalidBoard)
        }
        case ch if '1' to '8' contains ch => {
          file += (ch - '0')
          if file > 8 then return Left(ParseFenError.InvalidBoard)
        }
        case ch => {
          // println(s"do $ch $file $rank")
          (Piece.fromChar(ch), File(file), Rank(rank))
            .mapN((p, f, r) => (Square.square(f, r), p))
            .match
              case Some(p) => pieces.addOne(p)
              case None    => return Left(ParseFenError.InvalidBoard)
          file += 1
        }
    Right(Board.fromMap(pieces.toMap))
