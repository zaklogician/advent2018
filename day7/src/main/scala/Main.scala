package advent

import scala.util.parsing.combinator._

import shapeless._
import shapeless.tag.@@

object Day7 extends RegexParsers {
    
  // PROBLEM DOMAIN //
  
  trait NodeTag { }
  type Node = Char @@ NodeTag
  def Node(x: Char): Node = tag[NodeTag][Char](x)
  case class Edge(source: Node, target: Node) {
    def toParseString: String =
      "Step " + source + " must be finished before step " + target + " can begin."
  }
  
  /**
   * Returns a `Node` that is not the target of any `Edge` in the given set.
   * If there are multiple such steps, it returns the first alphabetically.
   * If there is no such step, it returns None.
   */
  def available(edges: Set[Edge]): Option[Node] = {
    val nodes: Set[Node] = edges.flatMap( e => List(e.source,e.target) )
    val targets: Set[Node] = edges.map(_.target)
    val candidates: Set[Node] = nodes diff targets
    if (candidates.isEmpty) None else Some {
      candidates.minBy(_.toChar)
    }
  }
  
  /**
   * Returns all the `Node`s in the given set of `Edge`s that are isolated
   * by the given `Node`, in the sense that they only occur as targets of
   * the given `Node`.
   */
  def isolatedBy(node: Node, edges: Set[Edge]): Set[Node] = {
    val sources: Set[Node] = edges.map(_.source)
    val nodeTargets: Set[Node] = edges.filter( e => e.source == node ).map(_.target)
    val otherTargets: Set[Node] = edges.filter( e => e.source != node ).map(_.target)
    (nodeTargets diff sources) diff otherTargets
  }
  
  
  /**
   * Represents the result of a topological sort of a graph.
   */
  sealed trait SortResult
  case class Sorted(sorting: List[Node]) extends SortResult
  case class Cyclic(edges: Set[Edge]) extends SortResult
  
  def sortState(sorting: List[Node], edges: Set[Edge]): SortResult =
    if (edges.isEmpty) Sorted(sorting.reverse) else available(edges) match {
      case None    => Cyclic(edges)
      case Some(c) => {
        val nextEdges = edges.filter(e => e.source != c)
        val isolated = isolatedBy(c, edges).toList.sortBy(_.toChar).reverse
        sortState(isolated ++ (c :: sorting), nextEdges)
      }
    }
  
  /**
   * Returns a topological sort of the `Node`s among the given set of `Edge`s.
   * Returns a witness to the graph being cyclic if no such sort exists.
   */
  def sort(edges: Set[Edge]): SortResult = sortState(Nil, edges)
  
  /**
   * Returns a string describing the order in which the given steps should be
   * completed. If more than one step is ready, chooses the step which is the
   * first alphabetically. Solves Part 1.
   */
  def part1(edges: Traversable[Edge]): String = {
    val edgeSet: Set[Edge] = Set() ++ edges
    sort(edgeSet) match {
      case Sorted(sorting) => sorting.mkString
      case Cyclic(_) => "cyclic graph"
    }
  }
  
  case class Job(node: Node, time: Int) {
    def step: Job = Job(node, time - 1)
    def isFinished: Boolean = time <= 0
  }
  def toJob(node: Node): Job = 
    Job(node, node.toChar.toInt - 4)
  
  /**
   * Returns the amount of time required to solve all tasks with the given number of workers.
   */
  def schedule(maxJobs: Int, edges:Set[Edge])(time: Int, inProgress: List[Job], finished: Set[Node]): Int = {
    // update finished
    val nextFinished: Set[Node] = finished ++ inProgress.map(_.step).filter(_.isFinished).map(_.node)
    
    // choose possible job candidates
    val nodes: Set[Node] = edges.flatMap( e => List(e.source,e.target) )
    val candidates: Set[Node] = for {
      n <- nodes
      if ( !inProgress.map(_.node).contains(n) )
      if ( !nextFinished.contains(n) )
      conds = edges.filter(e => e.target == n)
      if (conds.forall(e => nextFinished contains e.source))
    } yield n
    val candidateList: List[Job] = candidates.toList.sortBy(_.toChar).map(toJob)
    
    // schedule the required number of new jobs
    val remainingJobs: List[Job] = inProgress.map(_.step).filter(!_.isFinished)
    val nextInProgress: List[Job] = remainingJobs ++ candidateList.take(maxJobs - remainingJobs.length)
    
    if (nextInProgress.isEmpty && candidates.isEmpty) time else
    schedule(maxJobs,edges)(time + 1, nextInProgress, nextFinished)
  }
  
  def part2(edges: Traversable[Edge]): Int = {
    schedule(5, Set() ++ edges)(0, List(), Set())
  }
  
  
  // INPUT HANDLING //
  
  def lineParser: Parser[Edge] = 
    ((literal("Step ") ~> """[A-Z]""".r <~ "must be finished before step ") ~
    ("""[A-Z]""".r <~ "can begin.")) ^^ { case s ~ t => Edge(Node(s.head), Node(t.head)) }
  
  def inputParser: Parser[List[Edge]] = rep(lineParser)
  
  // MAIN //
  
  def main(args: Array[String]) = {
    val source = scala.io.Source.fromFile("input.txt")
    val content = try source.mkString finally source.close
    
    parse(inputParser, content) match {
      case Failure(msg,_)     => println("Parsing failed: " + msg)
      case Error(msg,_)       => println("Error: " + msg)
      case Success(matched,_) => {
        println( "Result of Part 1: " + part1(matched) )
        println( "Result of Part 2: " + part2(matched) )
      }
    }
    println("Bye!")
  }
  
}