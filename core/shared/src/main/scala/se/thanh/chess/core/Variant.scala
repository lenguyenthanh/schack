package se.thanh.chess.core

// enum BState:
//   case Standard
//   case CrazyHouse
//
// enum Variant[+S <: BState]:
//   case Standard extends Variant[BState.Standard.type]
//   case Chess960 extends Variant[BState.Standard.type]
//   case CrazyHouse extends Variant[BState.CrazyHouse.type]

// type Elem[X] = X match
//   case String => Char
//   case Array[t] => t
//   case Iterable[t] => t

enum Variant:
  case Standard
  case Atomic
  case Antichess
  case KingOfTheHill
  case ThreeCheck
  case Crazyhouse
  case RacingKings
  case Horde

  def uci: String = this match
    case Standard => "chess"
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
    case "atomic" => Some(Atomic)
    case "antichess" => Some(Antichess)
    case "kingofthehill" => Some(KingOfTheHill)
    case "threecheck" => Some(ThreeCheck)
    case "crazyhouse" => Some(Crazyhouse)
    case "racingkings" => Some(RacingKings)
    case "horde" => Some(Horde)
    case _ => None
