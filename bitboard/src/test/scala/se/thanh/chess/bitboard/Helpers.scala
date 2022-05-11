package se.thanh.chess.bitboard

import org.lichess.compression.game.Board as CBoard
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
