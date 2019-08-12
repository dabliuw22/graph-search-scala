package com.leysoft.astar

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.{PriorityQueue, Set}
import scala.math.{abs, pow, sqrt}
import util.control.Breaks._

class AstarSearch {

  def search(source: Node, target: Node): (Boolean, List[Node]) = {
    var search = false
    val explored: Set[Node] = Set.empty
    var unxplored = PriorityQueue[Node]()(AstarSearch.ordering)
    source.h = AstarSearch.manhattan(source, target)
    source.f = source.g + source.h
    unxplored.enqueue(source)

    while (!unxplored.isEmpty && !search) {
      val current = unxplored.dequeue
      explored.add(current)
      AstarSearch.logger.info(s"Current node: $current")
      if(current.value.equals(target.value)) {
        AstarSearch.logger.info("Eureka")
        search = true
      } else {
        AstarSearch.logger.info(s"${current.neighbors}")
        for (edge <- current.neighbors) {
          val child = edge.node
          val weight = edge.weight
          val g = current.g + weight
          val h = AstarSearch.manhattan(child, target)
          val f = g + h
          breakable {
            if (explored.contains(child) && f >= child.f) {
              break
            } else if (!unxplored.exists(node => node.equals(child)) || f < child.f) {
              child.parent = Some(current)
              child.g = g
              child.h = h
              child.f = f
              if (unxplored.exists(node => node.equals(child)))
                unxplored = unxplored.filterNot(node => node.equals(child))
              unxplored.enqueue(child)
            }
          }
        }
      }
    }
    (search, result(target))
  }

  private def result(target: Node): List[Node] = {
    var result = List[Node]()
    var node: Option[Node] = Some(target)
    while (node.isDefined) {
      result = result.appended(node.get)
      node = node.get.parent
    }
    result.reverse
  }
}

object AstarSearch {

  val logger = Logger(LoggerFactory.getLogger("AstarSearch"))

  val ordering: Ordering[Node] = (n1: Node, n2: Node) => -n1.f.compare(n2.f)

  // Manhattan distance
  val manhattan = (n1: Node, n2: Node) => abs(n2.x - n1.x) + abs(n2.y - n1.y)

  // Euclidean distance
  val euclidean = (n1: Node, n2: Node) => sqrt(pow(n2.x - n1.x, 2) + pow(n2.y - n1.y, 2))

  def apply(): AstarSearch = new AstarSearch()
}

case class Node(value: String, x: Double, y: Double, var g: Double = 0, var h: Double = 0,
                var f: Double = 0, var parent: Option[Node] = None, var block: Boolean = false)  {

  var neighbors: List[Edge] = List.empty

  def add(edge: Edge): Unit = neighbors = neighbors.appended(edge)

  override def toString: String = s"Node($value, $x, $y, $g, $h, $f)"

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[Node]
    value.equals(other.value) && x.equals(other.x) && y.equals(other.x)
  }
}

case class Edge(node: Node, var weight: Double = 0)

object Astar extends App {
  val logger = Logger(LoggerFactory.getLogger("Astar"))

  val a = Node("a", 1, 2)
  val b = Node("b", 3, 0)
  val c = Node("c", -2, -2)
  val d = Node("d", 0, 0)
  val e = Node("e", 5, 3)
  val f = Node("f", 3, 6)

  a.add(Edge(b, 3)); a.add(Edge(c, 3))
  b.add(Edge(c, 4)); b.add(Edge(d, 3))
  c.add(Edge(e, 7))
  d.add(Edge(e, 3)); d.add(Edge(f, 6))
  e.add(Edge(f, 2))

  val result = AstarSearch().search(a, e)
  logger.info(s"Search success: ${result._1}, result: ${result._2}")
}
