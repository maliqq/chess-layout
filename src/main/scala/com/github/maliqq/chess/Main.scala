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
    val count = layout.place(pieces)

    if (count == 0) {
      Console println("There is no solution")
      return
    }

    Console printf("Solution with %s layouts (including symmetric)\n", count)
  }
}
