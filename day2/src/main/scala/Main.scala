package advent

import scala.collection.SortedMap
import scala.util.parsing.combinator._

object Day2 extends RegexParsers {
  // PROBLEM DOMAIN //
  
  val ALPHABET: List[Char] = "abcdefghijklmnopqrstuvwxyz".toList
  
  /**
   * Represents a multiset of alphabetical characters.
   * 
   * `BoxID`s are elements of the free monoid on 26 generators, while `CharCount`s
   * are elements of `\mathbb{N}^26`, so we can let the obvious map from the 
   * 26-fold free product to the 26-fold Cartesian product do the counting.
   */
  class CharCount(fromSortedMap: SortedMap[Char,Int]) {
    val toMap: SortedMap[Char,Int] = fromSortedMap
    
    val toList: List[Int] = ALPHABET.map { x => toMap.getOrElse(x,0) }
    
    /**
     * Returns the pointwise sum of two `CharCounts`.
     */
    def +(that: CharCount) = new CharCount(
      SortedMap[Char,Int]() ++ ALPHABET.map { x => x -> (this.toMap(x) + that.toMap(x)) } 
    )
     
    lazy val hasTwoRepeated: Boolean = toMap.values.count(_ == 2) > 0
    lazy val hasThreeRepeated: Boolean = toMap.values.count(_ == 3) > 0
    
    override def equals(that: Any): Boolean = {
      that.isInstanceOf[CharCount] &&
      this.toList == that.asInstanceOf[CharCount].toList
    }
  }
  object CharCount {  
    val empty: CharCount = new CharCount(
      SortedMap[Char,Int]() ++ ALPHABET.map { x => x -> 0 }
    )
    
    def from(c: Char): CharCount = new CharCount(
      empty.toMap + (c -> 1)
    )
  }
  
  
  case class BoxID(asString: String) {
    require( asString.forall( c => ALPHABET.contains(c) ) )
    
    lazy val charCount: CharCount = 
      asString.toList.map(CharCount.from).foldLeft(CharCount.empty)(_ + _)

    /**
     * Finds all possible Hamming distance 1 neighbors of the `BoxID`.
     */
    def neighbors: Seq[BoxID] = for {
      n <- 0 to asString.length
      c <- ALPHABET
    } yield BoxID( asString.take(n) + c + asString.drop(n+1) ) 
  }
  
  
  /**
   * Calculates the checksum of a given list of `BoxID`s, by counting the number that have
   * an ID containing exactly two of any letter and then separately counting those with
   * exactly three of any letter, and multiplying th counts together. Solves Part 1.
   */
  def checksum(ids: Traversable[BoxID]): Int =
    ids.count(_.charCount.hasTwoRepeated) * ids.count(_.charCount.hasThreeRepeated)
    
  
  /**
   * Finds two boxes in the given list which differ by exactly one character at the same
   * position in both strings, or `None` if no such pair exists. Solves Part 2.
   * 
   * Note: this runs in linear time!
   */
  def findPrototype(ids: Traversable[BoxID]): Option[(BoxID, BoxID)] = {
    val idSet: Set[BoxID] = Set() ++ ids
    val results = for {
      id <- ids
      neighbors = id.neighbors.filter(n => n != id && idSet.contains(n))
      if (!neighbors.isEmpty)
    } yield (id,neighbors.head)
    results.headOption
  }
  
  // INPUT HANDLING //
  
  def lineParser: Parser[BoxID] =
    """[a-z]+""".r ^^ { x => BoxID(x) }
  
  def inputParser: Parser[List[BoxID]] = rep(lineParser)
  
  
  // MAIN //
  def main(args: Array[String]) = {
    val source = scala.io.Source.fromFile("input.txt")
    val content = try source.mkString finally source.close
    
    parse(inputParser, content) match {
      case Failure(msg,_)     => println("Parsing failed: " + msg)
      case Error(msg,_)       => println("Error: " + msg)
      case Success(matched,_) => {
        val part1 = checksum(matched)
        println( "Result of Part 1: " + part1 )
        val part2 = findPrototype(matched).map(x => x._1.asString intersect x._2.asString)
        println( "Result of Part 2: " + part2 )
      }
    }
    println("Bye!")
  }
  
}