import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

val lines = Source.fromFile("input.txt").getLines().toList

var digits = ArrayBuffer[Array[Char]]()
for ( line <- lines ) {
  val chars  = line.toArray
  digits += chars
}

// Part 1
var gamma = 0
var epsilon = 0
var value = 1
val rows = digits.transpose.reverse
for (row <- rows) {
  val one_size = row.filter(_ == '1').size
  val zero_size = row.filter(_ == '0').size
  if (one_size > zero_size) {
    gamma += value
  } else {
    epsilon += value
  }
  value *= 2
}

println(gamma * epsilon)

// Part 2
def getDecimal(binary : Array[Char]) = {
  var result = 0
  var value = 1
  for (i <- 0 until binary.size) {
    if (binary(i) == '1') {
      result += value
    }
    value *= 2
  }
  result
}

val digits_cp = digits
val sz = digits(0).size

var oxygen = 0
breakable {
  for (i <- 0 until sz) {
    if (digits.filter(x => x(i) == '1').size >= digits.filter(x => x(i) == '0').size) {
      digits = digits.filter(x => x(i) == '1')
    } else {
      digits = digits.filter(x => x(i) == '0')
    }
    if (digits.size == 1) {
      //oxygen = getDecimal(digits(0).reverse)
      oxygen = Integer.parseInt(digits(0).mkString, 2)
      break()
    }
  }
}

var digits = digits_cp
var carbon = 0
breakable {
  for (i <- 0 until sz) {
    if (digits.filter(x => x(i) == '0').size <= digits.filter(x => x(i) == '1').size) {
      digits = digits.filter(x => x(i) == '0')
    } else {
      digits = digits.filter(x => x(i) == '1')
    }
    if (digits.size == 1) {
      //carbon = getDecimal(digits(0).reverse)
      carbon = Integer.parseInt(digits(0).mkString, 2)
      break()
    }
  }
}

println(oxygen * carbon)
