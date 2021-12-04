import scala.io.Source
val lines = Source.fromFile("input.txt").getLines().toList.map(_.toInt)

// Part 1
var count = 0
val sz = lines.length - 1
for ( i <- 0 until sz ) {
    if (lines(i) < lines(i + 1)) {
        count += 1
    }
}
println(count)

// Part 2
var count = 0
val sz = lines.length - 3
for ( i <- 0 until sz ) {
  val sum1 = lines(i) + lines(i+1) + lines(i+2)
  val sum2 = lines(i+1) + lines(i+2) + lines(i+3)
  if (sum1 < sum2) {
    count += 1
  }
}
println(count)

// Refactor attempt #1
def advent_day1(n: Int) = {
  val lines_roll = lines.sliding(n).map(_.sum).toList
  var count = 0
  val sz = lines_roll.size - 1
  for ( i <- 0 until sz ) {
    if (lines_roll(i) < lines_roll(i + 1)) {
      count += 1
    }
  }
  count
}

scala> advent_day1(1)
val res7: Int = 1154

scala> advent_day1(3)
val res8: Int = 1127

// Refactor attempt #2
def advent_day1(n: Int) = {
  val lines_roll = lines.sliding(n).map(_.sum).toList
  val lines_tail = lines_roll.tail
  val lines_roll_shift = lines_roll.zip(lines_tail)
  lines_roll_shift.filter(x => x._2 > x._1).size
}
