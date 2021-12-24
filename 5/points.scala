import scala.io.Source
import scala.collection.mutable.Map

// pass by ref on map
def updateMap(x:Int, y:Int, map:Map[(Int, Int), Int]) : Unit = {
  if (map.contains((x, y))) {
    map((x, y)) += 1
  } else {
    map += ((x, y) -> 1)
  }
}

val lines = Source.fromFile("input.txt").getLines().toList

val pointsMap = Map[(Int, Int), Int]()
for (line <- lines) { 
  val points = line.split(" -> ").flatMap(_.split(',')).map(_.toInt)
  if (points(0) == points(2)) { // Part 1
    val x = points(0)
    val y_min = Math.min(points(1), points(3))
    val y_max = Math.max(points(1), points(3))
    for (y <- y_min to y_max) {
      updateMap(x, y, pointsMap)
    }
  } else if (points(1) == points(3)) { // Part 1
    val y = points(1)
    val x_min = Math.min(points(0), points(2))
    val x_max = Math.max(points(0), points(2))
    for (x <- x_min to x_max) {
      updateMap(x, y, pointsMap)
    }
  } else if ( (points(3) - points(1)) / (points(2) - points(0)) == 1) { // Part 2
    val x_min = Math.min(points(0), points(2))
    val x_max = Math.max(points(0), points(2))
    val y_min = Math.min(points(1), points(3))
    var y = y_min
    for (x <- x_min to x_max) {
      updateMap(x, y, pointsMap)
      y += 1
    }
  } else { // Part 2
    val x_min = Math.min(points(0), points(2))
    val x_max = Math.max(points(0), points(2))
    val y_max = Math.max(points(1), points(3))
    var y = y_max
    for (x <- x_min to x_max) {
      updateMap(x, y, pointsMap)
      y -= 1
    }
  }
}

println(pointsMap.filter(_._2 >= 2).size)







