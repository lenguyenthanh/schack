package se.thanh.chess.core

import cats.syntax.all.*
import munit.{ FunSuite, ScalaCheckSuite }
import org.scalacheck.Prop

import Square.*
import Arbitraries.given
import Square.*

class SquareTests extends ScalaCheckSuite:

  test("square") {
    for
      f <- 0 to 7
      r <- 0 to 7
      s        = (File(f), Rank(r)).mapN(Square.square)
      expected = Square(f + r * 8)
    yield assertEquals(s, expected)
  }

  test("square => file, rank => square") {
    (0 to 63).foreach { s =>
      val square = Square(s).get
      val f      = File.fromSquare(square)
      val r      = Rank.fromSquare(square)
      assertEquals(Square.square(f, r), square)
    }
  }

  test("mirror perseveres file") {
    (0 to 63).foreach { s =>
      val square = Square(s).get
      val mirror = square.mirror
      assertEquals(mirror.file, square.file)
    }
  }

  test("mirror transforms rank") {
    (0 to 63).foreach { s =>
      val square = Square(s).get
      val mirror = square.mirror
      assertEquals(mirror.rank + square.rank, 7)
    }
  }

  test("mirror.mirror == identity") {
    (0 to 63).foreach { s =>
      val square = Square(s).get
      assertEquals(square.mirror.mirror, square)
    }
  }

  test("a.combine(a) == a") {
    (0 to 63).foreach { s =>
      val square = Square(s).get
      assertEquals(square.combine(square), square)
    }
  }

  property("combine preserve file") {
    Prop.forAll { (square: Square, other: Square) =>
      val combined = square.combine(other)
      combined.file == square.file
    }
  }

  property("combine preserve rank of other") {
    Prop.forAll { (square: Square, other: Square) =>
      val combined = square.combine(other)
      combined.rank == other.rank
    }
  }
