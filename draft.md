# draft

A complete Game
  - A fix Setup
  - A variant
  - List of Move


A Setup
  - Board ( or a position)
  - State
    - turn: White|Black
    - castles
    - enpassant square: Option[Square]
    - fullMoves: UInt
    - halfMoves: Int
    - promoted: Bitboard (CrazyHouse)
    - pockets: CrazyHouse

legalMoves: Variant => Setup => List[Move]

# Variant priority

Std => 960 => crazyhouse

Variant => Game => parser

# Variant
- Name
- Description
- starting position
  - 960 has multiple starting positions
- different state

## Variant list

  - Standard
  - Chess960
  - Atomic
  - Antichess
  - KingOfTheHill
  - ThreeCheck
  - Crazyhouse
  - RacingKings
  - Horde
