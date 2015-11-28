package com.github.maliqq.chess

object Main {

  def main(args: Array[String]) {
    import Pieces._

    var (m, n) = (6, 9)
    var pieces = List[Piece](King(), Queen(), Queen(), Rook(), Bishop(), Knight())

    if (args.size == 2) {
      val Array(boardSize, piecesStr, _*) = args
      val Array(_m, _n) = boardSize.split('x')
      m = Integer.parseInt(_m)
      n = Integer.parseInt(_n)
      pieces = Pieces.parse(piecesStr).toList
    }

    val layout = new Layout(m, n)
    val paths = layout.place(pieces.sortBy(_.weight).reverse)

    if (paths.size == 0) {
      Console println("There is no solution")
      return
    }

    Console printf("Solution with %s layouts (including symmetric)\n", paths.size)

    val limit = 5
    if (paths.size > limit) Console printf("First %s are below:\n", limit)
    else Console println("All layouts are below:")

    implicit def path2board(p: Path): Board = {
      val b = new Board(m, n)
      p.foreach { case ((x, y), piece) => b.set(x, y, piece) }
      b
    }

    paths.take(limit).foreach { path =>
      val board = path: Board
      Console println(board.toString)
    }
  }
}
