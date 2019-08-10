package com.leysoft.idfs

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

class IterativeDepthFirstSearch {

}

object IterativeDepthFirstSearch {

  val logger = Logger(LoggerFactory.getLogger("IterativeDepthFirstSearch"))

  def apply: IterativeDepthFirstSearch = new IterativeDepthFirstSearch()
}

case class Node(data: String, var level: Int, var visited: Boolean) {

  var neighbors: List[Node] = List.empty

  def add(node: Node): Unit = neighbors = neighbors.appended(node)
}
