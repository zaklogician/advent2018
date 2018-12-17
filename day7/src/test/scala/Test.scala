package advent

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

import scala.util.parsing.combinator._
import advent.Day7._

object Test extends Properties("Main") {
  
  // GENERATORS //
  val genNode: Gen[Node] = for {
    x <- Gen.choose('A','Z')
  } yield Node(x)
  implicit val arbNode: Arbitrary[Node] = Arbitrary(genNode)
  
  
  val genEdge: Gen[Edge] = for {
    src <- genNode
    tgt <- genNode
  } yield Edge(src,tgt)
  implicit val arbEdge: Arbitrary[Edge] = Arbitrary(genEdge)
  
  // PROPERTIES //
  
  property("available is None iff every node is a target") = Prop.forAll { (es: Set[Edge]) =>
    val targets = es.map(_.target)
    available(es) match {
      case None    => es.forall(e => targets.contains(e.source) )
      case Some(c) => es.forall(e => e.target != c)
    }
  }
  
  property("sort respects test case for Part 1") = Prop {
    val A: Node = Node('A')
    val B: Node = Node('B')
    val C: Node = Node('C')
    val D: Node = Node('D')
    val E: Node = Node('E')
    val F: Node = Node('F')
    val edges = Set( Edge(C,A), Edge(C,F), Edge(A,B), Edge(A,D), Edge(B,E), Edge(D,E), Edge(F,E) )
    sort(edges) == Sorted( List(C,A,B,D,F,E) )
  }
  
  property("lineParser inverts toParseString") = Prop.forAll { (x: Edge) =>
    parse(lineParser, x.toString) match {
      case Success(matched,_) => matched == x
      case elze               => false
    }
  } // end property
  
}