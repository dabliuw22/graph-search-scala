package com.leysoft.dfs

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.Stack

class DepthFirstSearch {

  def search(root: Node, target: String): Boolean = {
    val stack: Stack[Node] = Stack.empty
    stack.push(root)
    root.visited = true

    while (!stack.isEmpty) {
      val current = stack.pop()
      DepthFirstSearch.logger.info(s"Current node: $current")
      if(current.data.equals(target)) {
        DepthFirstSearch.logger.info("Eureka")
        return true
      } else {
        for(node <- current.neighbors) {
          if(!node.visited) {
            node.visited = true
            stack.push(node)
          }
        }
      }
    }
    false
  }
}

object DepthFirstSearch {

  val logger = Logger(LoggerFactory.getLogger("DepthFirstSearch"))

  def apply(): DepthFirstSearch = new DepthFirstSearch()
}

case class Node(data: String, var visited: Boolean = false) {

  var neighbors: List[Node] = List.empty

  def add(node: Node): Unit = neighbors = neighbors.appended(node)
}

object Dfs extends App {
  val logger = Logger(LoggerFactory.getLogger("Dfs"))
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

  val dfs = DepthFirstSearch()
  val result = dfs.search(a, target)
  logger.info(s"Search success: $result")
}