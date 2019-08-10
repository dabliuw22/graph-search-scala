package com.leysoft.idfs

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import scala.collection.mutable.Stack
import util.control.Breaks._

class IterativeDepthFirstSearch {

  def search(root: Node, target: String, level: Int): Boolean = {
    val stack: Stack[Node] = Stack.empty
    stack.push(root)
    root.visited = true

    while (!stack.isEmpty) {
      val current = stack.pop
      current.visited = true
      IterativeDepthFirstSearch.logger.info(s"Current node: $current")
      if(current.data.equals(target)) {
        IterativeDepthFirstSearch.logger.info("Eureka")
        return true
      } else {
        breakable {
          if(current.level >= level) break
          for (node <- current.neighbors) {
            if(!node.visited && !stack.contains(node)) {
              node.level = current.level + 1
              stack.push(node)
            }
          }
        }
      }
    }

    false
  }
}

object IterativeDepthFirstSearch {

  val logger = Logger(LoggerFactory.getLogger("IterativeDepthFirstSearch"))

  def apply(): IterativeDepthFirstSearch = new IterativeDepthFirstSearch()
}

case class Node(data: String, var level: Int = 0, var visited: Boolean = false) {

  var neighbors: List[Node] = List.empty

  def add(node: Node): Unit = neighbors = neighbors.appended(node)
}

object Idfs extends App {
  val logger = Logger(LoggerFactory.getLogger("Idfs"))
  var a = Node("a"); val b = Node("b"); val c = Node("c")
  var d = Node("d"); val e = Node("e"); val f = Node("f")
  var g = Node("g"); val h = Node("h"); val i = Node("i")
  var j = Node("j"); val k = Node("k")

  a.add(d); a.add(c); a.add(b)
  b.add(f); b.add(a)
  c.add(f); c.add(e)
  d.add(i); d.add(c); d.add(a)
  e.add(g); e.add(f)
  f.add(h); f.add(b)
  g.add(j); g.add(k)
  i.add(j)
  j.add(k)
  k.add(g)

  val target = "g"

  val idfs = IterativeDepthFirstSearch()
  val result = idfs.search(a, target, 4)
  logger.info(s"Search success: $result")
}