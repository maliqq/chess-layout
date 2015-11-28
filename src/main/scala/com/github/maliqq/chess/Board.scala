package com.github.maliqq.chess

object Board {
  object Cell {
    final val White = '░'
    final val Black = '▒'
  }
}

trait AsciiPrint {
  def m: Int
  def n: Int
  def get(x: Int, y: Int): Option[Piece]
  def isAimed(x: Int, y: Int): Boolean
  override def toString = {
    val s = new StringBuffer
    for (y <- 0 to n-1) {
      for (x <- 0 to m-1) {
        val cell = get(x, y)
        if (cell.isDefined) {
          s.append(if (x % 2 != y % 2) cell.get.white else cell.get.black)
        } else if(isAimed(x, y)) {
          s.append('x')
        } else {
          s.append(if (x % 2 != y % 2) Board.Cell.White else Board.Cell.Black)
        }
        s.append(' ')
      }
      s.append('\n')
    }
    s.toString
  }
}

case class Put(x: Int, y: Int, piece: Piece)

class Board(val m: Int, val n: Int) extends Aiming with AsciiPrint {
  private val matrix: Array[Array[Option[Piece]]] = Array.fill(m) {
    Array.fill(n) { None }
  }
  val pieces: collection.mutable.ListBuffer[Put] = collection.mutable.ListBuffer()

  def isValid(x: Int, y: Int) = x >= 0 && y >= 0 && x < m && y < n
  def isEmpty(x: Int, y: Int) = matrix(x)(y).isEmpty
  def allEmpty(moves: Array[Position]) = moves.forall { case (x, y) => isEmpty(x, y) }
  def isTaken(x: Int, y: Int) = matrix(x)(y).isDefined
  def get(x: Int, y: Int): Option[Piece] = matrix(x)(y)
  def set(x: Int, y: Int, piece: Piece) { matrix(x)(y) = Some(piece) }

  def size = m * n
  def pieceNum = pieces.size
  def emptyNum = emptyCells.size
  def isFull = emptyNum == 0

  def emptyCells = {
    for {
      y <- 0 to n - 1;
      x <- 0 to m - 1
      if (isEmpty(x, y) && !isAimed(x, y))
    } yield((x, y))
  }.toArray

  def hasPiece(x: Int, y: Int, other: Piece): Boolean = get(x, y).map { piece =>
    piece == other
  }.getOrElse(false)

  def put(x: Int, y: Int, piece: Piece, moves: Array[Position]): Boolean = {
    if (!canPut(x, y, moves)) return false
    matrix(x)(y) = Some(piece)
    pieces += Put(x, y, piece)
    aim(x, y, piece, moves)
    true
  }

  def reset(x: Int, y: Int, moves: Array[Position]) {
    val piece: Piece = matrix(x)(y).get
    matrix(x)(y) = None
    piece match {
      case _: Moves.Direct =>
        aimedColumns(x).decr()
        aimedRows(y).decr()
      case _ =>
    }
    pieces.remove(pieces.indexOf(Put(x, y, piece)))
    moves.foreach { case (x, y) =>
      aimedCells(x)(y).decr()
    }
  }

  def canPut(x: Int, y: Int, moves: Array[Position]) = isEmpty(x, y) && !isAimed(x, y) && allEmpty(moves)

  def aim(x: Int, y: Int, piece: Piece, moves: Array[Position]) {
    piece match {
      case _: Moves.Direct =>
        aimedColumns(x).incr()
        aimedRows(y).incr()
      case _ =>
    }
    moves.foreach { case (x, y) =>
      aimedCells(x)(y).incr()
    }
  }

}
