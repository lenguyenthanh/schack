package se.thanh.chess.core

opaque type File <: Int = Int
opaque type Rank <: Int = Int

opaque type Square <: Int = Int

object File:
  def apply(i: Int): Option[File] =
    if (i >= 0 && i < 8) Some(i)
    else None

  def fromSquare(s: Square): File = s & 7

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

  def fromSquare(s: Square): Rank = s >>> 3

  val one: Rank = 0
  val two: Rank = 1
  val three: Rank = 2
  val four: Rank = 3
  val five: Rank = 4
  val six: Rank = 5
  val seven: Rank = 6
  val eight: Rank = 7

object Square:
  def apply(i: Int): Option[Square] =
    if (i >= 0 && i < 64) Some(i)
    else None

  def square(file: File, rank: Rank): Square =
    file ^ (rank << 3)

  extension (s: Square)
    def file: File = File.fromSquare(s)
    def rank: Rank = Rank.fromSquare(s)

    // mirror vertically
    def mirror: Square = s ^ 0x38

    def combine(o: Square) =
      square(s.file, o.rank)

    def aligned(o: Square): Boolean = ???
