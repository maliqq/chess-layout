package com.github.maliqq.chess

object Main {
  def main(args: Array[String]) {
    import Pieces._

    var board = new Board(6, 9)
    var pieces = List[Piece](new King(), new Queen(), new Queen(), new Rook(), new Bishop(), new Knight())

    if (args.size == 2) {
      val Array(boardSize, piecesStr, _*) = args
      val Array(m, n) = boardSize.split('x')
      board = new Board(Integer.parseInt(m), Integer.parseInt(n))
      pieces = Pieces.parse(piecesStr).toList
    }

    val layout = new Layout(board)
    val boards = layout.place(pieces)
    Console printf("Solution with %s layouts (including symmetric)\n", boards.size)

    val n = 5
    if (boards.size > n) Console printf("First %s are below:\n", n)
    else Console println("All layouts are below:")

    boards.take(n).foreach { board =>
      Console println(board.toString)
    }
  }
}
