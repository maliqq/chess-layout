package com.github.maliqq.chess

class Layout(val board: Board) {
  class NoSolutionException extends Exception

  type Path = List[Tuple2[Position, Piece]]
  def place(pieces: List[Piece]): List[Board] = {
    place(pieces.size, List.empty, pieces.sortBy(_.weight).reverse) match {
      case Some(paths) =>
        paths.map { path =>
          val b = new Board(board.m, board.n)
          path.foreach { case ((x, y), piece) => b.put(x, y, piece) }
          b
        }.foldRight(List[Board]()) { case (b, uniq) =>
         if (uniq.exists(_.equals(b))) uniq else b :: uniq
        }
      case None => List.empty
    }
  }

  def place(numReach: Int, path: Path, pieces: List[Piece]): Option[List[Path]] = {
    if (pieces.isEmpty) {
      if (path.size != numReach) return None
      else return Some(List(path))
    }
    if (board.isFull) return None

    val piece = pieces.head
    Some(
      board.emptyCells.foldLeft[List[Path]](List.empty) { case (result, (x, y)) =>
        if (board.canPut(x, y, piece)) {
          board.put(x, y, piece)
          place(numReach, Tuple2((x, y), piece) :: path, pieces.tail) match {
            case None =>
              board.reset(x, y)
              result
            case Some(l) =>
              board.reset(x, y)
              result ++ l
          }
        } else result
      }
    )
  }
}
