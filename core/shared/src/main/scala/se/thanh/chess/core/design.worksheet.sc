import cats.syntax.all.*

enum Color:
  case White, Black

enum Role:
  case King, Queen, Rook, Bishop, Knight, Pawn

case class Piece(color: Color, role: Role)

enum File:
  case A, B, C, D, E, F, G, H

enum Rank:
  case One, Two, Three, Four, Five, Six, Seven, Eight

case class Square(file: File, rank: Rank)

// Board: Map[Square, Piece]
trait Board:
  def getPiece(square: Square): Option[Piece]

case class Position[State](board: Board, state: State)

// Action = Move | Drop | whateveer
// trait Action

trait Variant[F[_]]:
  type Action
  type Position
  val name: String
  val description: String
  def move(position: Position)(action: Action): F[Position]
  def legalMoves(position: Position): F[List[Action]]
