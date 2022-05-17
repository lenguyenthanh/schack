package se.thanh.chess.core

import cats.syntax.all.*

opaque type File <: Int = Int
opaque type Rank <: Int = Int

opaque type Square <: Int = Int

object File:
  def apply(i: Int): Option[File] =
    if (i >= 0 && i < 8) Some(i)
    else None

  def apply(c: Char): Option[File] =
    apply(c - 'a')

  def fromSquare(s: Square): File = s & 7

  def char(f: File): Char = ('a' + f).toChar

  val a: File = 0
  val b: File = 1
  val c: File = 2
  val d: File = 3
  val e: File = 4
  val f: File = 5
  val g: File = 6
  val h: File = 7

object Rank:
  def apply(i: Int): Option[Rank] =
    if (i >= 0 && i < 8) Some(i)
    else None

  def apply(c: Char): Option[File] =
    apply(c - '1')

  def fromSquare(s: Square): Rank = s >>> 3

  def char(r: Rank): Char = ('1' + r).toChar

  val first: Rank = 0
  val second: Rank = 1
  val third: Rank = 2
  val fourth: Rank = 3
  val fifth: Rank = 4
  val sixth: Rank = 5
  val seventh: Rank = 6
  val eighth: Rank = 7

object Square:

  val a1: Square = 0
  val b1: Square = 1
  val c1: Square = 2
  val d1: Square = 3
  val e1: Square = 4
  val f1: Square = 5
  val g1: Square = 6
  val h1: Square = 7

  val a2: Square = 8
  val b2: Square = 9
  val c2: Square = 10
  val d2: Square = 11
  val e2: Square = 12
  val f2: Square = 13
  val g2: Square = 14
  val h2: Square = 15

  val a3: Square = 16
  val b3: Square = 17
  val c3: Square = 18
  val d3: Square = 19
  val e3: Square = 20
  val f3: Square = 21
  val g3: Square = 22
  val h3: Square = 23

  val a4: Square = 24
  val b4: Square = 25
  val c4: Square = 26
  val d4: Square = 27
  val e4: Square = 28
  val f4: Square = 29
  val g4: Square = 30
  val h4: Square = 31

  val a5: Square = 32
  val b5: Square = 33
  val c5: Square = 34
  val d5: Square = 35
  val e5: Square = 36
  val f5: Square = 37
  val g5: Square = 38
  val h5: Square = 39

  val a6: Square = 40
  val b6: Square = 41
  val c6: Square = 42
  val d6: Square = 43
  val e6: Square = 44
  val f6: Square = 45
  val g6: Square = 46
  val h6: Square = 47

  val a7: Square = 48
  val b7: Square = 49
  val c7: Square = 50
  val d7: Square = 51
  val e7: Square = 52
  val f7: Square = 53
  val g7: Square = 54
  val h7: Square = 55

  val a8: Square = 56
  val b8: Square = 57
  val c8: Square = 58
  val d8: Square = 59
  val e8: Square = 60
  val f8: Square = 61
  val g8: Square = 62
  val h8: Square = 63

  val all = List(
    a1, b1, c1, d1, e1, f1, g1, h1,
    a2, b2, c2, d2, e2, f2, g2, h2,
    a3, b3, c3, d3, e3, f3, g3, h3,
    a4, b4, c4, d4, e4, f4, g4, h4,
    a5, b5, c5, d5, e5, f5, g5, h5,
    a6, b6, c6, d6, e6, f6, g6, h6,
    a7, b7, c7, d7, e7, f7, g7, h7,
    a8, b8, c8, d8, e8, f8, g8, h8,
    )

  def apply(i: Int): Option[Square] =
    if (i >= 0 && i < 64) Some(i)
    else None

  def square(file: File, rank: Rank): Square =
    file ^ (rank << 3)

  def fromString(s: String): Option[Square] =
    s.toList match
      case x::y::Nil => (File(x), Rank(y)).mapN(square)
      case _ => None

  extension (s: Square)
    def file: File = File.fromSquare(s)
    def rank: Rank = Rank.fromSquare(s)

    def uci: String =
      val f = File.char(s.file)
      val r = Rank.char(s.rank)
      s"$f$r"

    // mirror vertically
    def mirror: Square = s ^ 0x38

    def combine(o: Square): Square =
      square(s.file, o.rank)
