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
  def pieces: collection.mutable.Map[Piece, Position]
  override def toString = {
    val s = new StringBuffer
    for (x <- 0 to m-1) {
      for (y <- 0 to n-1) {
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

class Board(val m: Int, val n: Int) extends Aiming with AsciiPrint {

  private val matrix: Array[Array[Option[Piece]]] = Array.fill(m) {
    Array.fill(n) { None }
  }
  val pieces: collection.mutable.Map[Piece, Position] = collection.mutable.Map.empty

  def isTaken(x: Int, y: Int) = matrix(x)(y).isDefined

  def size = m * n
  def pieceNum = pieces.size
  def emptyNum = emptyCells.size
  def isFull = emptyNum == 0

  def emptyCells = {
    for {
      x <- 0 to m - 1;
      y <- 0 to n - 1
      if (!isTaken(x, y) && !isAimed(x, y))
    } yield((x, y))
  }.toArray

  def get(x: Int, y: Int): Option[Piece] = matrix(x)(y)

  def put(x: Int, y: Int, piece: Piece): Boolean = {
    if (isTaken(x, y) || isAimed(x, y)) return false
    matrix(x)(y) = Some(piece)
    pieces(piece) = (x, y)
    aim(x, y, piece)
    true
  }

  def canPut(x: Int, y: Int) = isValid(x, y) && !isTaken(x, y) && !isAimed(x, y)

  def aim(x: Int, y: Int, piece: Piece) {
    piece match {
      case _: Moves.Direct =>
        aimedColumns(x) = true
        aimedRows(y) = true
      case _ =>
    }
    piece match {
      case diag: Moves.Diagonal =>
        diag.diagonalMove(x, y, this).foreach { case (x, y) =>
          aimedCells(x)(y) = true
        }
      case _ =>
    }
    piece match {
      case _: Moves.Diagonal | _: Moves.Direct =>
      case _ =>
        piece.moves(x, y, this).foreach { case (x, y) =>
          aimedCells(x)(y) = true
        }
    }
  }

  def reset(x: Int, y: Int) {
    val piece: Piece = matrix(x)(y).get
    pieces.remove(piece)
    matrix(x)(y) = None
    aimedColumns(x) = false
    aimedRows(y) = false
    for {
      x <- 0 to m - 1;
      y <- 0 to n - 1
    } aimedCells(x)(y) = false
    pieces.foreach { case (piece, (x, y)) =>
      aim(x, y, piece)
    }
  }

  def isValid(x: Int, y: Int) = x >= 0 && y >= 0 && x < m && y < n

  def isEmpty(x: Int, y: Int) = matrix(x)(y).isEmpty

}
