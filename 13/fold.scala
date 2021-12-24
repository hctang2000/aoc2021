import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

//can't use these functions, vis is problematic globally
def foldy(y:Int) = {
  for (i <- y+1 until vis.size) {
    val row = i - (2 * (i - y))
    for ( x <- 0 until vis(0).size) vis(row)(x) += vis(i)(x)
    //for ( x <- 0 until vis(0).size) vis(i)(x) = 0
  }
}

def foldx(x:Int) = {
  for (j <- x+1 until vis(0).size) {
    val col = j - (2 * (j - x))
    for ( y <- 0 until vis.size) vis(y)(col) += vis(y)(j)
    //for ( y <- 0 until vis.size) vis(y)(j) = 0
  }
}

def counting() = {
  var count = 0
  for (i <- 0 until vis.size) {
    for (j <- 0 until vis(0).size) {
      if (vis(i)(j) > 0) count += 1
    }
  }
  count
}

//val lines = Source.fromFile("input.txt").getLines().toList
val lines = Source.fromFile("example.txt").getLines().toList

var xs = List[Int]()
var ys = List[Int]()

breakable {
  for (line <- lines) {
    if(line.isEmpty) {
      break()
    }

    val pair = line.split(',')
    xs = pair(0).toInt::xs
    ys = pair(1).toInt::ys
  }
}

val rows = ys.max+1
val cols = xs.max+1

var vis = Array.fill(rows, cols){0}

while (!xs.isEmpty) {
  val y = xs.head
  val x = ys.head

  vis(x)(y) = 1
  xs = xs.tail
  ys = ys.tail
}

var start = false

breakable {
  for (line <- lines) {
    if (line.isEmpty) {
      start = true
    }
    else if (start) {
      val words = line.split(' ')
      val ops = words(2).split('=')
      if (ops(0) == "y") {
        val y = ops(1).toInt

        for (i <- y+1 until vis.size) {
          val row = i - (2 * (i - y))
          for ( x <- 0 until vis(0).size) vis(row)(x) += vis(i)(x)
        }

        /* ArrayBuffer can call remove which is in place
         val sz = vis.size -1
        for (i <- sz to y by -1) {
          vis.remove(i)
        }
         */

        //with ArrayBuffer, one can do vis.sliceInPlace(0,  y)
        vis = vis.slice(0,  y)

        /*
        var count = 0
        for (i <- 0 until vis.size) {
          for (j <- 0 until vis(0).size) {
            if (vis(i)(j) > 0) count += 1
          }
        }
         */

        println(counting())
        //break()
      } else {
        val x = ops(1).toInt
        for (j <- x+1 until vis(0).size) {
          val col = j - (2 * (j - x))
          for ( y <- 0 until vis.size) {
            vis(y)(col) += vis(y)(j)
          }
         }

        /*
         val sz = vis(0).size - 1
        for (j <- sz to x by -1) {
          for ( y <- 0 until vis.size)
            vis(y).remove(j)
        }
         */

        vis = vis.map(_.slice(0, x))

        /*
        var count = 0
        for (i <- 0 until vis.size) {
          for (j <- 0 until vis(0).size) {
            if (vis(i)(j) > 0) count += 1
          }
        }
         */

        println(counting())
        //break()
      }
    }
  }
}

for (i <- 0 until vis.size) {
  for (j <- 0 until vis(0).size) {
    if (vis(i)(j) > 0) print("#")
    else print(".")
  }
  println()
}



