import scala.io.Source
val lines = Source.fromFile("input.txt").getLines().toList

//Part 1
var x = 0
var y = 0
for ( line <- lines ) {
  val movement = line.split(' ')
  movement(0) match {
    case "forward" => x += movement(1).toInt
    case "down" => y += movement(1).toInt
    case "up" => y -= movement(1).toInt
  }
}
println(x * y)

// Part 2
var x = 0
var y = 0
var aim = 0
for ( line <- lines ) {
  val movement = line.split(' ')
  movement(0) match {
    case "forward" => {
      x += movement(1).toInt
      y += aim * movement(1).toInt
    }
    case "down" => aim += movement(1).toInt
    case "up" => aim -= movement(1).toInt
  }
}
println(x * y)
