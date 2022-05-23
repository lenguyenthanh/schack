package se.thanh.chess.core

import se.thanh.chess.bitboard.Board
import se.thanh.chess.bitboard.State
import se.thanh.chess.bitboard.StandardMovesGenerator.*
import se.thanh.chess.bitboard.Fen

enum BState:
  case Standard
  case CrazyHouse
  case Chess960

// enum Variant[+S <: BState]:
//   case Standard extends Variant[BState.Standard.type]
//   case Chess960 extends Variant[BState.Standard.type]
//   case CrazyHouse extends Variant[BState.CrazyHouse.type]

type VariantState[X <: Variant] = X match
  case Variant.Standard.type => State
  case Variant.Crazyhouse.type => BState.CrazyHouse.type
  case _ => BState.Standard.type

trait Pos( val v: Variant, val state: VariantState[v.type])

case class Setup[V <: Variant](board: Board, state: VariantState[V])
class Setup2(v: Variant, board: Board, state: VariantState[v.type]):
  def legalMoves: List[Move] =
    v match
      case Variant.Standard => Fen(board, state).generate
      case _ => List()

enum Variant:
  case Standard
  case Chess960
  case Atomic
  case Antichess
  case KingOfTheHill
  case ThreeCheck
  case Crazyhouse
  case RacingKings
  case Horde

  def uci: String = this match
    case Standard => "chess"
    case Chess960 => "chess960"
    case Atomic => "atomic"
    case Antichess => "antichess"
    case KingOfTheHill => "kingofthehill"
    case ThreeCheck => "threecheck"
    case Crazyhouse => "crazyhouse"
    case RacingKings => "racingkings"
    case Horde => "horde"

object Variant:

  import Variant.*
  def fromUci: String => Option[Variant] =
    case "chess" => Some(Standard)
    case "chess960" => Some(Chess960)
    case "atomic" => Some(Atomic)
    case "antichess" => Some(Antichess)
    case "kingofthehill" => Some(KingOfTheHill)
    case "threecheck" => Some(ThreeCheck)
    case "crazyhouse" => Some(Crazyhouse)
    case "racingkings" => Some(RacingKings)
    case "horde" => Some(Horde)
    case _ => None
