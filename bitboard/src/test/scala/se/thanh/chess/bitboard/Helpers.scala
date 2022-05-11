package se.thanh.chess.bitboard

import org.lichess.compression.game.Board as CBoard
import org.lichess.compression.game.MoveList
import org.lichess.compression.game.Move as CMove
import scala.collection.mutable.ListBuffer

object Helpers:
  extension (f: Fen)
    def cBoard: CBoard =
      CBoard(
        f.board.pawns,
        f.board.knights,
        f.board.bishops,
        f.board.rooks,
        f.board.queens,
        f.board.kings,
        f.board.white,
        f.board.black,
        f.isWhiteTurn,
        f.state.epSquare.getOrElse(0),
        f.state.castlingRights
      )

  extension (ml: MoveList)
    def uciSet: Set[String] =
      val buffer = ListBuffer[String]()
      (0 until ml.size).foreach(i => buffer.addOne(ml.get(i).uci))
      buffer.toSet
