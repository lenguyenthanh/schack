# draft

A complete Game
  - A fix Setup (a starting position)
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

# Question
- How can I define chess rule as Value
  - Is a position valid
  - End of game condition
    - Draw
    - able to claim draw
    - stalemate
    - checkmated
    - Not enough material
- How can I define variant as Value
- Variant is a set of rule and all possible positions
- What is a state

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
  - Differentiate between Valid Position and Invalid position
