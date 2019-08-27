package com.leysoft.ucost

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.collection.mutable.{PriorityQueue, Set}
import scala.util.control.Breaks.{break, breakable}

class UniformCostSearch {

  def search(source: Node, target: Node): (Boolean, List[Node]) = {
    var search = false
    val explored: Set[Node] = Set.empty
    var unxplored = PriorityQueue[Node]()(UniformCostSearch.ordering)
    unxplored.enqueue(source)

    while (!unxplored.isEmpty && !search) {
      val current = unxplored.dequeue
      explored.add(current)
      UniformCostSearch.logger.info(s"Current node: $current")
      if (current.value.equals(target.value)) {
        search = true
      } else {
        for (edge <- current.neighbors) {
          val child = edge.node
          val weight = edge.weight
          val f = current.f + weight
          breakable {
            if (explored.contains(child) && f >= child.f) {
              break
            } else if (!unxplored.exists(node => node.equals(child)) || f < child.f) {
              child.parent = Some(current)
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

object UniformCostSearch {

  val logger = Logger(LoggerFactory.getLogger("UniformCostSearch"))

  val ordering: Ordering[Node] = (n1: Node, n2: Node) => -n1.f.compare(n2.f)

  def apply(): UniformCostSearch = new UniformCostSearch()
}

case class Node(value: String, var f: Double = 0, var parent: Option[Node] = None) {

  var neighbors: List[Edge] = List.empty

  def add(edge: Edge): Unit = neighbors = neighbors.appended(edge)

  override def toString: String = s"Node($value, $f)"

  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[Node]
    value.equals(other.value)
  }
}

case class Edge(node: Node, var weight: Double = 0)

object UniformCost extends App {
  val logger = Logger(LoggerFactory.getLogger("UniformCost"))

  val a = Node("a")
  val b = Node("b")
  val c = Node("c")
  val d = Node("d")
  val e = Node("e")
  val f = Node("f")

  a.add(Edge(b, 3)); a.add(Edge(c, 3))
  b.add(Edge(c, 4)); b.add(Edge(d, 3))
  c.add(Edge(e, 7))
  d.add(Edge(e, 3)); d.add(Edge(f, 6))
  e.add(Edge(f, 2))

  val result = UniformCostSearch().search(a, e)
  logger.info(s"Search success: ${result._1}, result: ${result._2}")
}

