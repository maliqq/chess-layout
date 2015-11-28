package com.github.maliqq.chess

class Layout(m: Int, n: Int) {
  import collection.JavaConversions._
  import java.util.concurrent._

  lazy val threadsNum = Runtime.getRuntime().availableProcessors()
  lazy val pool = Executors.newFixedThreadPool(threadsNum)

  def sort(pieces: List[Piece]) = pieces.sortBy(_.weight).reverse

  def place(_pieces: List[Piece]): Int = {
    val pieces = sort(_pieces)

    val board = new Board(m, n)
    val piece = pieces.head
    val empty = board.emptyCells

    val counter = new atomic.AtomicInteger()
    val done = new CountDownLatch(empty.size)
    empty.foreach { case (x, y) =>
      pool.execute(new Runnable {
        override def run() {
          val copy = new Board(m, n)
          val moves = piece.moves(x, y, copy)
          copy.put(x, y, piece, moves)
          val count = place(copy, List(((x, y), piece)), pieces.tail)
          counter.addAndGet(count)
          done.countDown()
        }
      })
    }
    done.await()
    pool.shutdown()

    counter.get()
  }

  def place(board: Board, path: Path, pieces: List[Piece]): Int = {
    val piece = pieces.head
    val ((prevx, prevy), prev) = path.head
    board.emptyCells.foldLeft[Int](0) { case (count, (x, y)) =>
      if (piece == prev && x * n + y < prevx * n + prevy) count // skip
      else {
        val moves = piece.moves(x, y, board)
        if (board.put(x, y, piece, moves)) {
          try {
            if (pieces.tail.isEmpty)
              count + 1 // no pieces left
            else if (board.isFull)
              count
            else
              count + place(board, ((x, y), piece) :: path, pieces.tail)
          } finally {
            board.reset(x, y, moves)
          }
        } else count
      }
    }
  }
}
