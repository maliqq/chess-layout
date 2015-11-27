package com.github.maliqq.chess

trait Aiming { b: Board =>
  val aimedColumns: collection.mutable.ListBuffer[Boolean] = collection.mutable.ListBuffer.fill(m) { false }
  val aimedRows: collection.mutable.ListBuffer[Boolean] = collection.mutable.ListBuffer.fill(n) { false }
  val aimedCells: Array[Array[Boolean]] = Array.fill(m) {
    Array.fill(n) { false }
  }
  def isAimed(x: Int, y: Int) = aimedColumns(x) || aimedRows(y) || aimedCells(x)(y)
  def notAimingAnyone(x: Int, y: Int, piece: Piece) = piece.moves(x, y, this).forall { case (x, y) => isEmpty(x, y) }
}
