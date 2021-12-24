import scala.io.Source
import scala.util.control.Breaks._

val surface = Source.fromFile("input.txt").getLines().toArray.map(_.toArray.map(_.asDigit))

val dx = Array(1, -1, 0, 0, -1, 1, -1, 1)
val dy = Array(0, 0, 1, -1, -1, -1, 1, 1)

val rows = surface.size
val cols = surface(0).size

val vis = Array.fill(rows, cols){false}

var acc = 0
def flash(x:Int, y:Int) : Unit = {
  vis(x)(y) = true
  acc += 1

  for ( k <- 0 to 7) {
    val xp = x + dx(k)
    val yp = y + dy(k)
    if ((xp >= 0 && xp < rows) && (yp >= 0 && yp < cols)) {
      surface(xp)(yp) += 1
      if (!vis(xp)(yp) && surface(xp)(yp) >= 10) {
        flash(xp, yp)
      }
    }
  }

  surface(x)(y) = 0
}

breakable {
  var i = 0
  //for (i <- 1 to 100) { // Part 1
  while (true) { //Part 2
    i += 1
    for ( x <- 0 until rows) {
      for ( y <- 0 until cols) {
        surface(x)(y) += 1
      }
    }
    for ( x <- 0 until rows) {
      for ( y <- 0 until cols) {
        if (surface(x)(y) >= 10 && !vis(x)(y)) {
          flash(x, y)
        }
      }
    }

    if (vis.forall(_.forall(_ == true))) { //Part 2
      println(i)
      break()
    }

    for ( x <- 0 until rows) {
      for ( y <- 0 until cols) {
        if (vis(x)(y)) {
          surface(x)(y) = 0
          vis(x)(y) = false
        }
      }
    }
  }
}

println(acc)






