import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

val lines = Source.fromFile("input.txt").getLines().toList
val line = lines(0).split(',').map(_.toInt)

val min = line.min
val max = line.max

var crab = Map[Int, Int]()
for (i <- line) {
  if (crab.contains(i)) {
    crab(i) += 1
  } else {
    crab(i) = 1
  }
}

// Part 1
var dists1 = ArrayBuffer[Int]()
for (i <- min to max) {
  var dist = 0
  for ((k, v) <- crab) {
    dist += (k - i).abs * v
  }
  dists1 += dist
}

println(dists1.min)


// Part 2
var dists2 = ArrayBuffer[Int]()
for (i <- min to max) {
  var dist = 0
  for ((k, v) <- crab) {
    dist += (1 to (k - i).abs).sum * v
  }
  dists2 += dist
}

println(dists2.min)
