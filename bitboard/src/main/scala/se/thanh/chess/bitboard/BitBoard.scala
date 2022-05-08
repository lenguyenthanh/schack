package se.thanh.chess.bitboard

import se.thanh.chess.core.Square

type Bitboard = Long

object Bitboard:

  val ALL   = -1L

  val RANKS = Array.fill(8)(0L)
  val FILES = Array.fill(8)(0L)

  val KNIGHT_DELTAS     = Array[Int](17, 15, 10, 6, -17, -15, -10, -6)
  val BISHOP_DELTAS     = Array[Int](7, -7, 9, -9)
  val ROOK_DELTAS       = Array[Int](1, -1, 8, -8)
  val KING_DELTAS       = Array[Int](1, 7, 8, 9, -1, -7, -8, -9)
  val WHITE_PAWN_DELTAS = Array[Int](7, 9)
  val BLACK_PAWN_DELTAS = Array[Int](-7, -9)

  val KNIGHT_ATTACKS     = Array.fill(64)(0L)
  val KING_ATTACKS       = Array.fill(64)(0L)
  val WHITE_PAWN_ATTACKS = Array.fill(64)(0L)
  val BLACK_PAWN_ATTACKS = Array.fill(64)(0L)

  val BETWEEN = Array.ofDim[Long](64, 64)
  val RAYS    = Array.ofDim[Long](64, 64)

  // Large overlapping attack table indexed using magic multiplication.
  val ATTACKS = Array.fill(88772)(0L)

  /** Slow attack set generation. Used only to bootstrap the attack tables.
    */
  def slidingAttacks(square: Int, occupied: Bitboard, deltas: Array[Int]): Bitboard =
    var attacks = 0L
    deltas.foreach { delta =>
      var sq: Int = square
      var i       = 0
      while
        i += 1
        // println(s"while $sq ${attacks.toHexString}")
        sq += delta

        // println(s"data $sq -> ${distance(sq, sq -delta)} -> ${occupied.contains(sq)}")

        val con = (sq < 0 || 64 <= sq || distance(sq, sq - delta) > 2)
        if (!con)
          attacks |= 1L << sq

        !(occupied.contains(sq) || con)
      do ()
    }
    attacks

  def initMagics(square: Int, magic: Magic, shift: Int, deltas: Array[Int]) =
    var subset = 0L
    while
      val attack = slidingAttacks(square, subset, deltas)
      val idx    = ((magic.factor * subset) >>> (64 - shift)).toInt + magic.offset
      ATTACKS(idx) = attack

      // Carry-rippler trick for enumerating subsets.
      subset = (subset - magic.mask) & magic.mask

      subset != 0
    do ()

  def init() =
    (0 until 8).foreach { i =>
      RANKS(i) = 0xffL << (i * 8)
      FILES(i) = 0x0101010101010101L << i
    }

    val squareRange = 0 until 64
    squareRange.foreach { sq =>
      // println(s"$sq")
      KNIGHT_ATTACKS(sq) = slidingAttacks(sq, Bitboard.ALL, KNIGHT_DELTAS)
      KING_ATTACKS(sq) = slidingAttacks(sq, Bitboard.ALL, KING_DELTAS)
      WHITE_PAWN_ATTACKS(sq) = slidingAttacks(sq, Bitboard.ALL, WHITE_PAWN_DELTAS)
      BLACK_PAWN_ATTACKS(sq) = slidingAttacks(sq, Bitboard.ALL, BLACK_PAWN_DELTAS)

      initMagics(sq, Magic.ROOK(sq), 12, ROOK_DELTAS)
      initMagics(sq, Magic.BISHOP(sq), 9, BISHOP_DELTAS)
    }

    for
      a <- squareRange
      b <- squareRange
      _ = if slidingAttacks(a, 0, ROOK_DELTAS).contains(b) then
        BETWEEN(a)(b) = slidingAttacks(a, 1L << b, ROOK_DELTAS) & slidingAttacks(b, 1L << a, ROOK_DELTAS)
        RAYS(a)(b) = (1L << a) | (1L << b) | slidingAttacks(a, 0, ROOK_DELTAS) & slidingAttacks(b, 0, ROOK_DELTAS)
      else if slidingAttacks(a, 0, BISHOP_DELTAS).contains(b) then
        BETWEEN(a)(b) = slidingAttacks(a, 1L << b, BISHOP_DELTAS) & slidingAttacks(b, 1L <<a, BISHOP_DELTAS)
        RAYS(a)(b) = (1L << a) | (1L << b) | slidingAttacks(a, 0, BISHOP_DELTAS) & slidingAttacks(b, 0, BISHOP_DELTAS)
    yield ()

  extension (b: Bitboard)
    def contains(s: Int): Boolean =
      (b & (1L << s)) != 0

  private def distance(a: Int, b: Int): Int =
    Math.max(Math.abs(a.file - b.file), Math.abs(a.rank - b.rank))

  extension (a: Int)
    private def file = a & 7
    private def rank = a >>> 3
