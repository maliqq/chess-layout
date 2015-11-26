package com.github.maliqq.chess

object Board {
}

class Board(val m: Int, val n: Int) {

  private val matrix: Array[Array[Option[Piece]]] = Array.tabulate(m) {
    Array.tabulate(n) { None }
  }

  def put(x: Int, y: Int, piece: Piece) {
    matrix(x)(y) = Some(piece)
  }

  def capture(x: Int, y: Int) {
    matrix(x)(y) = None
  }

  def isVacant(x: Int, y: Int): Boolean = matrix(x)(y).isEmpty

}
