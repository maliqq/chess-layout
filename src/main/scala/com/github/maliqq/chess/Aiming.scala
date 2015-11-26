package com.github.maliqq.chess

trait Aiming { b: Board =>
  val aimedColumns: collection.mutable.ListBuffer[Boolean] = collection.mutable.ListBuffer.fill(m) { false }
  val aimedRows: collection.mutable.ListBuffer[Boolean] = collection.mutable.ListBuffer.fill(n) { false }
  val aimedCells: Array[Array[Boolean]] = Array.fill(m) {
    Array.fill(n) { false }
  }
  def isAimed(x: Int, y: Int) = aimedColumns(x) || aimedRows(y) || aimedCells(x)(y)

  def aimedNum: Int = {
    aimedColumns.filter(_ == true).size * m +
    aimedRows.filter(_ == true).size * n +
    aimedCellsNum
  }

  def aimedCellsNum: Int = {
    for {
      x <- 0 to m - 1;
      y <- 0 to n - 1
      if (!aimedColumns(x) && !aimedRows(y) && aimedCells(x)(y))
    } yield(x, y)
  }.size
}
