package se.thanh.chess.core

opaque type File <: Int = Int
opaque type Rank <: Int = Int

opaque type Square <: Int = Int

object File:
  def apply(i: Int): Option[File] =
    if (i >= 0 && i < 8) Some(i)
    else None

  def fromSquare(s: Square): File = s & 7

object Rank:
  def apply(i: Int): Option[Rank] =
    if (i >= 0 && i < 8) Some(i)
    else None
  def fromSquare(s: Square): Rank = s >>> 3

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
