import scala.io.Source
import scala.collection.mutable.Map

val lines = Source.fromFile("input.txt").getLines().toList

// Part 1
var count = 0
for (line <- lines) {
  val pair = line.split("\\Q | ")
  val output = pair(1).split(' ')
  for (elem <- output) {
    if (elem.size == 2 || elem.size == 3 ||
      elem.size == 4 || elem.size == 7) {
      count += 1
    }
  }
}

println(count)

// Part 2
//val freq_map = Map('a'->8, 'b'->6, 'c'->8, 'd'->7, 'e'->4, 'f'->9, 'g'->7)

val digits = Map[Set[Char], Char]()
digits("abcefg".toSet) = '0'
digits("cf".toSet) = '1'
digits("acdeg".toSet) = '2'
digits("acdfg".toSet) = '3'
digits("bcdf".toSet) = '4'
digits("abdfg".toSet) = '5'
digits("abdefg".toSet) = '6'
digits("acf".toSet) = '7'
digits("abcdefg".toSet) = '8'
digits("abcdfg".toSet) = '9'

var sum = 0

for (line <- lines) {
  var input_map = Map[Char, Int]()
  val pair = line.split("\\Q | ")
  val _input = pair(0)
  val _output = pair(1)
  for (c <- _input) {
    if (c != ' ') {
      if (input_map.contains(c)) {
        input_map(c) += 1
      } else {
        input_map(c) = 1
      }
    }
  }

  val map = Map[Char, Char]() //scrambled letter -> original letter
  for ((k, v) <- input_map) {
    if (v == 4) {
      map(k) = 'e'
    } else if (v == 6) {
      map(k) = 'b'
    } else if (v == 9) {
      map(k) = 'f'
    }
  }

  val _strs = _input.split(' ')
  val strs = _strs.sortBy(_.size)
  for (str <- strs) {
    if (str.size == 2) { // digit 1
      for (s <- str) {
        if (!map.contains(s)) {
          map(s) = 'c'
        }
      }
    } else if (str.size == 3) { // digit 7 
      for (s <- str) {
        if (!map.contains(s)) {
          map(s) = 'a'
        }
      }
    } else if (str.size == 4) { // digit 4
      for (s <- str) {
        if (!map.contains(s)) {
          map(s) = 'd'
        }
      }
    } else if (str.size == 7) { // digit 8
      for (s <- str) {
        if (!map.contains(s)) {
          map(s) = 'g'
        }
      }
    }
  }

  val output = _output.split(' ')
  var result = ""
  for (str <- output) {
    var s = ""
    for (c<-str) {
      s += map(c)
    }
    result += digits(s.toSet)
  }

  sum += result.toInt
}

println(sum)


