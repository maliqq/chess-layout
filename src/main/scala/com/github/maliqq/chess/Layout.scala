package com.github.maliqq.chess

class Layout(m: Int, n: Int) {
  class NoSolutionException extends Exception

  import collection.JavaConversions._
  import java.util.concurrent._

  lazy val workersNum = Runtime.getRuntime().availableProcessors()
  lazy val pool = Executors.newFixedThreadPool(workersNum)

  def place(pieces: List[Piece]): List[Path] = {
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
        }
      })
    }
    done.await()
    pool.shutdown()

    val indexes = new collection.mutable.HashSet[Long]()
    val result = q.toList.flatten
    Console printf("Total (not uniqie): %s\n", result.size)
    result.filter { p =>
      val hashCode = java.util.Arrays.hashCode(
        p.map { case ((x, y), piece) =>
          (x, y, piece.code)
        }.sorted.map { case (x, y, w) => List[Int](x, y, w) }.flatten.toArray
      )

      if (indexes.contains(hashCode)) false
      else {
        indexes.add(hashCode)
        true
      }
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
            if (pieces.tail.isEmpty) {
              newPath :: result // no pieces left
            } else if (board.isFull)
              result
            else
              place(board, newPath, pieces.tail) ::: result
          } finally {
            board.reset(x, y, moves)
          }
        } else result
      }
    }
    place(board, path, pieces)
  }
}
