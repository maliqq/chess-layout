package com.github.maliqq

package object chess {
  type Path = List[Tuple2[Position, Piece]]
  type Position = Tuple2[Int, Int]
}
