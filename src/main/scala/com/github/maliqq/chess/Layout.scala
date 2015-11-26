package com.github.maliqq.chess

class Layout(val board: Board) {
  class NoSolutionException extends Exception

  type Path = List[Tuple2[Position, Piece]]
  def place(pieces: List[Piece]): List[Board] = {
    place(List.empty, pieces.sortBy(_.weight).reverse) match {
      case Some(paths) =>
        return paths.flatMap { path =>
          if (path.size == pieces.size) {
            val b = new Board(board.m, board.n)
            path.foreach { case ((x, y), piece) =>
              b.put(x, y, piece)
            }
            Some(b)
          } else None
        }
      case None => return List.empty
    }
  }

  def place(path: Path, pieces: List[Piece]): Option[List[Path]] = {
    if (pieces.isEmpty) return Some(List(path))
    if (board.isFull) return None
    val piece = pieces.head
    Some(
      board.emptyCells.foldLeft[List[Path]](List.empty) { case (result, (x, y)) =>
        if (board.put(x, y, piece)) {
          place(Tuple2((x, y), piece) :: path, pieces.tail) match {
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
