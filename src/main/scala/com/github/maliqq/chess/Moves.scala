package com.github.maliqq.chess

object Moves {

  trait Diagonal {
    def diagonalMove(x: Int, y: Int, board: Board): Array[Position] = {
      val moves = collection.mutable.ArrayBuffer[Position]()
      for (moveX <- 0 to board.m - 1) {
        if (moveX != x) {
          val offsetX = x - moveX
          val upperY = y - offsetX
          val lowerY = y + offsetX
          if (board.canPut(moveX, upperY)) moves += Tuple2(moveX, upperY)
          if (board.canPut(moveX, lowerY)) moves += Tuple2(moveX, lowerY)
        }
      }
      moves.toArray
    }
  }

  trait Direct {
    def directMove(x: Int, y: Int, board: Board): Array[Position] = {
      val horizontal = for (moveX <- 0 to board.m - 1 if moveX != x && !board.aimedColumns(x)) yield((moveX, y))
      val vertical = for (moveY <- 0 to board.n - 1 if moveY != y && !board.aimedRows(y)) yield((y, moveY))
      (horizontal ++ vertical).toArray
    }
  }

}
