import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

val lines = Source.fromFile("input.txt").getLines().toList

var surface = ArrayBuffer[Array[Int]]()
for (line <- lines) {
  val arr = line.toArray.map(_ - '0')
  surface += arr
}

val dx = Array(1, -1, 0, 0)
val dy = Array(0, 0, 1, -1)

val rows = surface.size
val cols = surface(0).size

var acc = 0
var basins = ArrayBuffer[Int]()
var vis = ArrayBuffer.fill(rows, cols){false}

def helper(x:Int, y:Int) : Unit = {
  vis(x)(y) = true
  acc += 1

  for ( k <- 0 to 3) {
    val xp = x + dx(k)
    val yp = y + dy(k)
    if ((xp >= 0 && xp < rows) && (yp >= 0 && yp < cols)) {
      if (!vis(xp)(yp) && surface(xp)(yp) != 9)
        helper(xp, yp)
    }
  }
}

def findBasin(x:Int, y:Int) = {
  vis(x)(y) = false

  acc = 0
  helper(x, y)
  basins += acc
}

var risk = 0
for ( x <- 0 until rows) {
  for ( y <- 0 until cols) {
    var isLow = true
    breakable {
      for ( k <- 0 to 3) {
        val xp = x + dx(k)
        val yp = y + dy(k)
        if ((xp >= 0 && xp < rows) && (yp >= 0 && yp < cols)) {
          if (surface(x)(y) >= surface(xp)(yp)) {
            isLow = false
            break() 
          }
        }
      }
    }
    if (isLow) {
      risk += surface(x)(y) + 1
      findBasin(x, y)
    }
  }
}


println(risk)
//println(basins.sorted.reverse.take(3).foldLeft(1)((acc, x) => acc * x))
println(basins.sorted.reverse.take(3).product)




