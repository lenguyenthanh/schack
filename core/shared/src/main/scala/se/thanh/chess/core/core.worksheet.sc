import se.thanh.chess.core.*
import se.thanh.chess.bitboard.*
import se.thanh.chess.bitboard.StandardMovesGenerator.*

// val bla = Setup[Variant.Standard.type](Board.standard, BState.Standard)
val bla = Setup[Variant.Standard.type](Board.standard, State.start)

println(bla.state)
def play[V <: Variant](p: Setup[V]) =
  println(p.state)

play(bla)

trait Chess[+S, M]:
  val board: Board
  val state: S
  def legalMoves: List[M]

// case class Std(override val board: Board, override val state: State) extends Chess[State, Move]:
//   def legalMoves = Fen(board, state).generate
// val std = Std(Board.standard, State.start)
// std.legalMoves

case class Chess960(override val board: Board, override val state: State) extends Chess[State, Move]:
  def legalMoves = Fen(board, state).generate

trait V[S, M]:
  val name: String
  val description: String
  def legalMoves(setup: Setup4[S]): List[M]

case class Setup4[S](val board: Board, val state: S)

object Std extends V[State, Move]:
  val name = "std"
  val description = "std"
  def legalMoves(setup: Setup4[State]) = Fen(setup.board, setup.state).generate

val s4 = Setup4(Board.standard, State.start)

Std.legalMoves(s4)

class Game[S, M](v: V[S, M], start: Setup4[S], moves: List[M])

val ss = Setup[Variant.Standard.type](Board.standard, State.start)
