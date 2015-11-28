package com.github.maliqq.chess

trait Aiming { b: Board =>

  protected class Counter {
    private var n = 0
    def incr() {
      n += 1
    }
    def decr() {
      n -= 1
    }
    def value = n
    def booleanValue: Boolean = n > 0
  }

  protected val aimedColumns: Array[Counter] = Array.fill(m) { new Counter }
  protected val aimedRows: Array[Counter] = Array.fill(n) { new Counter }
  protected val aimedCells: Array[Array[Counter]] = Array.fill(m) {
    Array.fill(n) { new Counter }
  }
  def isAimed(x: Int, y: Int) = aimedColumns(x).booleanValue || aimedRows(y).booleanValue || aimedCells(x)(y).booleanValue
}
