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

    def combine(o: Square) =
      square(s.file, o.rank)

    def aligned(o: Square): Boolean = ???
