import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set

def isWinner(arr : ArrayBuffer[Array[Int]]) : Boolean = {
  for (i <- 0 until 5) {
    if ( arr(i).sum == 5 ) {
      return true
    }
  }
  val arr_t = arr.transpose
  for (i <- 0 until 5) {
    if ( arr_t(i).sum == 5 ) {
      return true
    }
  }

  false
}

def getSum(arr : ArrayBuffer[Array[Int]], barr: ArrayBuffer[Array[Int]]) = {
  var sum = 0
  for (i <- 0 until 5) {
    for (j <- 0 until 5) {
      if (barr(i)(j) == 0) {
        sum += arr(i)(j)
      }
    }
  }

  sum
}

def winFirst(flag : Boolean) : Unit = {
  val lines = Source.fromFile("input.txt").getLines().toList
  val len = lines.size

  val numbers = lines(0).split(',').map(_.toInt)

  var boards = ArrayBuffer[ArrayBuffer[Array[Int]]]()
  var board = ArrayBuffer[Array[Int]]() // Bingo Board

  var bboards = ArrayBuffer[ArrayBuffer[Array[Int]]]()
  var bboard = ArrayBuffer[Array[Int]]() // Board with 0 or 1

  for ( i <- 2 until len ) {
    if (lines(i) != "") {
      val nums  = lines(i).split(' ').filter(!_.isEmpty).map(_.toInt)
      board += nums

      bboard += Array(0, 0, 0, 0, 0)
    } else {
      boards += board.clone
      board.clear()

      bboards += bboard.clone()
      bboard.clear()
    }
  }

  val sz = boards.size
  var win_set = Set[Int]()
  for (num <- numbers) {
    for (index <- 0 until sz) {
      for (i <- 0 until 5) {
        for (j <- 0 until 5) {
          if (boards(index)(i)(j) == num) {
            bboards(index)(i)(j) = 1
            if (isWinner(bboards(index))) {
              win_set += index
              val sum = getSum(boards(index), bboards(index))
              if (flag) {
                println(num * sum)
                return
              } else {
                if (win_set.size == sz) {
                  println(num * sum)
                  return
                }
              }
            }
          }
        }
      }
    }
  }
}

winFirst(true) //Part 1, win first board
winFirst(false) //Part 2, win last board





