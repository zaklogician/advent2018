package advent

import scala.util.parsing.combinator._

import shapeless._
import shapeless.tag.@@

object Day3 extends RegexParsers {
    
  // PROBLEM DOMAIN //
  
  trait ClaimIdTag {}
  type ClaimId = Int @@ ClaimIdTag
  def ClaimId(x: Int): ClaimId = tag[ClaimIdTag][Int](x)
  
  trait CornerTag {}
  type Corner = (Int,Int) @@ CornerTag
  def Corner(x: Int, y: Int): Corner = tag[CornerTag][(Int,Int)]((x,y))
  
  trait ExtentTag {}
  type Extent = (Int,Int) @@ ExtentTag
  def Extent(x: Int, y: Int): Extent = tag[ExtentTag][(Int,Int)]((x,y))
  
  trait PointTag {}
  type Point = (Int,Int) @@ PointTag // (~ indexed square inch of fabric)
  def Point(x: Int, y: Int): Point = tag[PointTag][(Int,Int)]((x,y))
  
  
  case class Claim(id: ClaimId, corner: Corner, extent: Extent) {
    override def toString: String = {
      "#" + id + " @ " + corner._1 + "," + corner._2 + ": " + extent._1 + "x" + extent._2
    }
    
    /** The list of points covered by this claim. */
    lazy val points: Set[Point] = {
      val pointList: Seq[Point] = for {
        x <- (corner._1 + 1) to (corner._1 + extent._1)
        y <- (corner._2 + 1) to (corner._2 + extent._2)
      } yield Point(x,y)
      Set() ++ pointList
    }
  }
  
  
  /**
   * A map that sends each point to the set of claims covering that point.
   * 
   * Sets of `Claim` form a commutative monoid `S` under the union operation.
   * All claims over the fabric can be represented as elements of
   * the monoid `T = S^{1000 \times 1000}`: we map each `Claim` to its
   * characteristic matrix in `T`, and then use the monoid operation
   * to determine which `Point`s are claimed by which `Claims`.
   */
  case class Partition(toMap: Map[ Point, Set[Claim] ]) {
    def +(that: Partition): Partition = Partition {
      this.toMap ++ that.toMap.keys.map { k => k -> (
        this.toMap.getOrElse(k,Set()) union that.toMap(k)
      )}
    }
  }
  
  object Partition {
    def empty: Partition = Partition( Map() )
    
    def from( claim: Claim ): Partition = Partition {
      Map() ++ claim.points.map { p => p -> Set(claim) }
    }
    
    def from( claims: Traversable[Claim] ): Partition = 
      claims.map(Partition.from).foldLeft(Partition.empty)((x,y) => x + y)
  }
  
  
  /**
   * Finds the total number of 1 square inch points which are covered by
   * more than one `Claim` in the given list. Solves Part 1.
   */
  def overlaps(claims: Traversable[Claim]): Int =
    Partition.from(claims).toMap.filter( x => x._2.size > 1 ).size
  
  
  /**
   * Finds a `Claim` that does not intersect any other claim in the given
   * list. Returns `None` if no such `Claim` exists. Solves Part 2.
   */
  def noverlaps(claims: Traversable[Claim]): Option[Claim] = {
    val part: Partition = Partition.from(claims)
    claims.dropWhile { c => 
      part.toMap.exists { x => x._2.contains(c) && x._2.size > 1 }
    }.headOption
  }
  
  // INPUT HANDLING //
  
  val idParser: Parser[ClaimId] = 
    ("""#[1-9]\d*""".r ~ "@") ^^ { case x ~ _ => ClaimId(x.tail.toInt) } 
  
  val cornerParser: Parser[Corner] = 
    ("""\d+""".r ~ "," ~ """\d+""".r ~ ":"  ) ^^ { case x ~ _ ~ y ~ _ => Corner(x.toInt,y.toInt) } 
  
  val extentParser: Parser[Extent] = 
    ("""\d+""".r ~ """x""".r ~ """\d+""".r  ) ^^ { case x ~ _ ~ y => Extent(x.toInt,y.toInt) } 
  
  def lineParser: Parser[Claim] =
    (idParser ~ cornerParser ~ extentParser) ^^ {
      case (id ~ c ~ e) => Claim( id, c, e )
    }
  
  def inputParser: Parser[List[Claim]] = rep(lineParser)
  
  // MAIN //
  
  def main(args: Array[String]) = {
    val source = scala.io.Source.fromFile("input.txt")
    val content = try source.mkString finally source.close
    
    parse(inputParser, content) match {
      case Failure(msg,_)     => println("Parsing failed: " + msg)
      case Error(msg,_)       => println("Error: " + msg)
      case Success(matched,_) => {
        println( "Result of Part 1: " + overlaps(matched) )
        println( "Result of Part 2: " + noverlaps(matched) )
      }
    }
    println("Bye!")
  }
  
}