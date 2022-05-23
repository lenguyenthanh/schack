import se.thanh.chess.core.*
import se.thanh.chess.bitboard.*

// val bla = Setup[Variant.Standard.type](Board.standard, BState.Standard)
val bla = Setup[Variant.Standard.type](Board.standard, State.start)

println(bla.state)
def play[V <: Variant](p: Setup[V]) =
  println(p.state)

play(bla)
