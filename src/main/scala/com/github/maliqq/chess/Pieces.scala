package com.github.maliqq.chess

abstract class Piece {
  def moves(x: Int, y: Int, board: Board): Array[Position]
  def weight: Int
  def white: Char
  def black: Char
  override def toString = black.toString
}

object Pieces {

  def parse(s: String): Seq[Piece] = {
    s.toCharArray.map { char =>
      char match {
        case 'K' | 'k' => new King()
        case 'Q' | 'q' => new Queen()
        case 'R' | 'r' => new Rook()
        case 'B' | 'b' => new Bishop()
        case 'S' | 's' | 'N' | 'n' => new Knight()
      }
    }
  }

  object King {
  }
  sealed class King extends Piece {
    def weight = 8
    def moves(x: Int, y: Int, board: Board) = {
      for {
        moveX <- (x - 1) to (x + 1);
        moveY <- (y - 1) to (y + 1)
        if (!(moveX == x && moveY == y) && board.canPut(moveX, moveY))
      } yield((moveX, moveY))
    }.toArray

    def white = '♔'
    def black = '♚'
  }

  object Queen {}
  sealed class Queen extends Piece with Moves.Diagonal with Moves.Direct {
    def weight = 10
    def moves(x: Int, y: Int, board: Board) = directMove(x, y, board) ++ diagonalMove(x, y, board)

    def white = '♕'
    def black = '♛'
  }

  object Rook {
  }
  sealed class Rook extends Piece with Moves.Direct {
    def weight = 9
    def moves(x: Int, y: Int, board: Board) = directMove(x, y, board)

    def white = '♖'
    def black = '♜'
  }

  object Bishop {
  }
  sealed class Bishop extends Piece with Moves.Diagonal {
    def weight = 9
    def moves(x: Int, y: Int, board: Board) = diagonalMove(x, y, board)

    def white = '♗'
    def black = '♝'
  }

  object Knight {}
  sealed class Knight extends Piece {
    def weight = 7
    final val Moves = Array[Position](
        (2, 1), (2, -1),
        (1, 2), (-1, 2),
        (-1, -2), (1, -2),
        (-2, -1), (-2, 1)
    )

    def moves(x: Int, y: Int, board: Board) = {
      for (
        (offsetX, offsetY) <- Moves;
        moveX = offsetX + x;
        moveY = offsetY + y
        if board.canPut(moveX, moveY)
      ) yield((moveX, moveY))
    }.toArray

    def white = '♘'
    def black = '♞'
  }

}
