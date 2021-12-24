import scala.io.Source
import scala.collection.mutable.Stack
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

val lines = Source.fromFile("input.txt").getLines().toList

val lefts = Set('(', '[', '{', '<')

var points = 0
val scores = ArrayBuffer[BigInt]()
for (line <- lines) {
  var stack = Stack[Char]()
  var isBad = false
  breakable { // Part 1
    for (c <- line) {
      if (lefts.contains(c)) {
        stack.push(c)
      } else {
        if (c == ')') {
          if (stack.top == '(') {
            stack.pop()
          } else {
            points += 3
            isBad = true
            break()
          }
        } else if (c == ']') {
          if (stack.top == '[') {
            stack.pop()
          } else {
            points += 57
            isBad = true
            break()
          }
        } else if (c == '}') {
          if (stack.top == '{') {
            stack.pop()
          } else {
            points += 1197
            isBad = true
            break()
          }
        } else if (c == '>') {
          if (stack.top == '<') {
            stack.pop()
          } else {
            points += 25137
            isBad = true
            break()
          }
        }
      }
    }
  }


  if (!isBad) { //Part 2
    var score:BigInt = 0
    while (!stack.isEmpty) {
      val c = stack.top
      if (c == '(') {
        score = score * 5 + 1
      } else if (c == '[') {
        score = score * 5 + 2
      } else if (c == '{') {
        score = score * 5 + 3
      } else if (c == '<') {
        score = score * 5 + 4
      }
      stack.pop()
    }
    scores += score
  }
}

println(points)
val _scores = scores.sorted
println(_scores(_scores.size/2))









