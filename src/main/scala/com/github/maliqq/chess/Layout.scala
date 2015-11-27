package com.github.maliqq.chess

class Layout(m: Int, n: Int) {
  class NoSolutionException extends Exception

  implicit def path2board(p: Path): RawBoard = new RawBoard(m, n, p)

  import collection.JavaConversions._
  import java.util.concurrent._

  lazy val workersNum = Runtime.getRuntime().availableProcessors()
  lazy val pool = Executors.newFixedThreadPool(workersNum)

  def place(pieces: List[Piece]): List[RawBoard] = {
    val board = new Board(m, n)
    val piece = pieces.head
    val q = new LinkedBlockingQueue[List[Path]]()
    val empty = board.emptyCells

    val done = new CountDownLatch(empty.size)
    empty.foreach { case (x, y) =>
      pool.execute(new Runnable {
        override def run() {
          val copy = new Board(m, n)
          val moves = piece.moves(x, y, copy)
          copy.put(x, y, piece, moves)
          val paths = worker(copy, List(((x, y), piece)), pieces.tail)
          q.put(paths)
          done.countDown()
          Console printf("STOP %s %s - %s%% done\n", (x, y), paths.size, (1 - done.getCount().toFloat / empty.size) * 100)
        }
      })
    }
    done.await()
    pool.shutdown()

    q.toList.flatten.map { path =>
      path: RawBoard
    }.foldRight(List[RawBoard]()) { case (b, uniq) =>
      if (uniq.exists(_.equals(b))) uniq else b :: uniq
    }
  }

  def worker(board: Board, path: Path, pieces: List[Piece]) = {
    def place(board: Board, path: Path, pieces: List[Piece]): List[Path] = {
      val piece = pieces.head
      board.emptyCells.foldLeft[List[Path]](List.empty) { case (result, (x, y)) =>
        val moves = piece.moves(x, y, board)
        val newPath = ((x, y), piece) :: path
        if (board.put(x, y, piece, moves)) {
          try {
            if (pieces.tail.isEmpty)
              newPath :: result // no pieces left
            else if (board.isFull)
              result
            else
              place(board, newPath, pieces.tail) ++ result
          } finally {
            board.reset(x, y, moves)
          }
        } else result
      }
    }
    place(board, path, pieces)
  }
}
