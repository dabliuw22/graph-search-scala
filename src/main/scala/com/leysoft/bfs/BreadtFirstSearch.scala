package com.leysoft.bfs

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.Queue

class BreadtFirstSearch {

  def search(root: Node, target: String): Boolean = {
    val queue: Queue[Node] = Queue.empty
    queue.enqueue(root)
    root.visited = true

    while (!queue.isEmpty) {
      val current = queue.dequeue
      BreadtFirstSearch.logger.info(s"Current node: $current")
      if(current.data.equals(target)) {
        BreadtFirstSearch.logger.info("Eureka")
        return true
      } else {
        for (node <- current.neighbors) {
          if(!node.visited) {
            node.visited = true
            queue.enqueue(node)
          }
        }
      }
    }
    false
  }
}

object BreadtFirstSearch {

  val logger = Logger(LoggerFactory.getLogger("BreadtFirstSearch"))

  def apply(): BreadtFirstSearch = new BreadtFirstSearch()
}

case class Node(data: String, var visited: Boolean = false) {

  var neighbors: List[Node] = List.empty

  def add(node: Node): Unit = neighbors = neighbors.appended(node)
}

object Bfs extends App {
  val logger = Logger(LoggerFactory.getLogger("Bfs"))
  var a = Node("a"); val b = Node("b"); val c = Node("c")
  var d = Node("d"); val e = Node("e"); val f = Node("f")
  var g = Node("g"); val h = Node("h"); val i = Node("i")
  var j = Node("j"); val k = Node("k")

  a.add(b); a.add(c); a.add(d)
  b.add(a); b.add(f)
  c.add(e); c.add(f)
  d.add(a); d.add(c); d.add(i)
  e.add(f); e.add(g)
  f.add(b); f.add(h)
  g.add(k); g.add(j)
  i.add(j)
  j.add(k)
  k.add(g)

  val target = "g"

  val bfs = BreadtFirstSearch()
  val result = bfs.search(a, target)
  logger.info(s"Search success: $result")
}
