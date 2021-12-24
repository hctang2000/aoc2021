import scala.io.Source
import scala.collection.mutable.Map

val lines = Source.fromFile("input.txt").getLines().toList
val line = lines(0).split(',').map(_.toInt)

def findFish(n: Int) = {
  var fish = Map[Int, BigInt]()
  for (i <- line) {
    if (fish.contains(i)) {
      fish(i) += 1
    } else {
      fish(i) = 1
    }
  }

  for (i <- 0 until n) {
    var map = Map[Int, BigInt]()
    for ((age, count) <- fish) {
      if (age != 0) {
        val _age =  age - 1
        if (map.contains(_age)) {
          map += _age -> (map(_age) + count)
        } else {
          map += _age -> count
        }
      } else {
          map += 6 -> count
          map += 8 -> count
      }
    }

    fish = map
  }

  fish.values.sum
}

println(findFish(80))
println(findFish(256))
