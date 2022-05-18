package se.thanh.chess.core

enum BState:
  case Standard
  case CrazyHouse

enum Variant[+S <: BState]:
  case Standard extends Variant[BState.Standard.type]
  case Chess960 extends Variant[BState.Standard.type]
  case CrazyHouse extends Variant[BState.CrazyHouse.type]

// type Elem[X] = X match
//   case String => Char
//   case Array[t] => t
//   case Iterable[t] => t
