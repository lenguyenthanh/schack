package se.thanh.chess

import org.scalacheck.{ Arbitrary, Gen }

object Arbitraries:

  given Arbitrary[Square] = Arbitrary(Gen.choose(0, 63).map(n => Square(n).get))

  given Arbitrary[File] = Arbitrary(Gen.choose(0, 7).map(n => File(n).get))
  given Arbitrary[Rank] = Arbitrary(Gen.choose(0, 7).map(n => Rank(n).get))
