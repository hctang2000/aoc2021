import scala.io.Source

val lines = Source.fromFile("input.txt").getLines().toList

var graph = Map[String, ArrayBuffer[String]]()

def addEdge(node1: String, node2: String) = {
  if (node2 != "start") {
  if (graph.contains(node1)) 
    graph(node1) += node2
  else {
      graph(node1) = ArrayBuffer(node2)
  }
  }

  if (node1 != "start") {
  if (graph.contains(node2)) 
    graph(node2) += node1
  else {

    graph(node2) = ArrayBuffer(node1)
  }
  }
}

for (line <- lines) {
  val nodes = line.split('-')
  addEdge(nodes(0), nodes(1))
}


var count = 0

def DFS(src: String, dst: String, path:List[String], vis:Set[String], weight: Int) : Unit = {

  if (src == dst) {
    count += 1
  } else {
    for (node <- graph(src)) {
      if (node.exists(_.isUpper)) {
        DFS(node, dst, node::path, vis, weight)
      } else if (!vis.contains(node)) {
        DFS(node, dst, node::path, vis.union(Set(node)), weight)
      } else if (weight > 0) {
         DFS(node, dst, node::path, vis.union(Set(node)), weight - 1)
      }
    }
  }

}

DFS("start", "end", List("start"), Set("start"), 0) //Part 1
DFS("start", "end", List("start"), Set("start"), 1) //Part 2

println(count)







