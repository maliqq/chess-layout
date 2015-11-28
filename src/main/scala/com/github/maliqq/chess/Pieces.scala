package com.github.maliqq.chess

abstract class Piece {
  def moves(x: Int, y: Int, board: Board): Array[Position]
  def weight: Int
  def white: Char
  def black: Char
  def code: Int
  override def toString = black.toString
}

object Pieces {

  def parse(s: String): Seq[Piece] = {
    s.toCharArray.map { char =>
      char match {
        case 'K' | 'k' => King()
        case 'Q' | 'q' => Queen()
        case 'R' | 'r' => Rook()
        case 'B' | 'b' => Bishop()
        case 'S' | 's' | 'N' | 'n' => Knight()
      }
    }
  }

  sealed case class King extends Piece {
    def weight = 8
    def moves(x: Int, y: Int, board: Board) = {
      for {
        moveX <- (x - 1) to (x + 1);
        moveY <- (y - 1) to (y + 1)
        if (!(moveX == x && moveY == y) && board.isValid(moveX, moveY))
      } yield((moveX, moveY))
    }.toArray

    def white = '♔'
    def black = '♚'
    def code: Int = 'K'
  }

  sealed case class Queen extends Piece with Moves.Diagonal with Moves.Direct {
    def weight = 10
    def moves(x: Int, y: Int, board: Board) = directMove(x, y, board) ++ diagonalMove(x, y, board)

    def white = '♕'
    def black = '♛'
    def code: Int = 'Q'
  }

  sealed case class Rook extends Piece with Moves.Direct {
    def weight = 9
    def moves(x: Int, y: Int, board: Board) = directMove(x, y, board)

    def white = '♖'
    def black = '♜'
    def code: Int = 'R'
  }

  sealed case class Bishop extends Piece with Moves.Diagonal {
    def weight = 9
    def moves(x: Int, y: Int, board: Board) = diagonalMove(x, y, board)

    def white = '♗'
    def black = '♝'
    def code: Int = 'B'
  }

  sealed case class Knight extends Piece {
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
        if board.isValid(moveX, moveY)
      ) yield((moveX, moveY))
    }.toArray

    def white = '♘'
    def black = '♞'
    def code: Int = 'N'
  }

}
