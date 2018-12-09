package advent

import shapeless.tag
import shapeless.tag.@@

object Day5 {
  
  // PROBLEM DOMAIN //
  
  trait PTypeTag { }
  type PType = Char @@ PTypeTag
  def PType(x: Char): PType = {
    require('a' <= x && x <= 'z')
    tag[PTypeTag][Char](x)
  }
  
  val PTYPES: List[PType] = "abcdefghijklmnopqrstuvwxyz".toList.map(PType)
  trait PUnitTag { } 
  type PUnit = Char @@ PUnitTag // a Polymer unit
  def PUnit(x: Char): PUnit = {
    require('A' <= x && x <= 'z')
    tag[PUnitTag][Char](x)
  }
  
  sealed trait Polarity { }
  case object Lower extends Polarity
  case object Upper extends Polarity
  
  def polarity(p: PUnit): Polarity =
    if (p.isLower) Lower else Upper
    
  def ptype(p: PUnit): PType = PType(p.toLower)
  
  def reactive(p: PUnit, q: PUnit): Boolean = 
    ptype(p) == ptype(q) && polarity(p) != polarity(q)
  
  
  type Polymer = List[PUnit]
  
  /**
   * Represents a `Polymer` that inductively maintains the invariant of being
   * in reduced form (i.e. having no reactive pairs).
   */
  case class Reduced(toList: List[PUnit]) {
    
    /** 
     * Appends the given `PUnit` to the underlying `Polymer`, maintaining
     * reduced form.
     */
    def +(p: PUnit): Reduced = toList match {
      case Nil => Reduced(List(p))
      case (r :: rs) =>
        if ( reactive(p,r) ) Reduced(rs)
        else Reduced(p :: r :: rs)
    }
  
    lazy val toPolymer: Polymer = toList.reverse
    override def toString: String = toPolymer.mkString
  }


  /**
   * Appends the given `Polymer` to the given `Reduced` polymer, 
   * effectively reducing the given `Polymer` in the process, since
   * the operations on `Reduced` maintain the required invariant.
   */
  def react(reduced: Reduced, polymer: Polymer): Reduced = polymer match {
    case Nil => reduced
    case (r :: rs) => react(reduced + r, rs)
  }
  
  def reduce(polymer: Polymer): Reduced =
    react( Reduced( List() ), polymer)
  
  /** 
   * Calculates the length of the `Reduced` poduced from the given `Polymer`
   * by fully reacting it. Solves Part 1.
   */
  def size(polymer: Polymer): Int =
    reduce(polymer).toPolymer.length
  
  /**
   * Calculates the length of the shortest `Reduced` one can produce from the given
   * `Polymer` by removing all `PUnit`s of exactly one type and fully reacting the 
   * remainder. Solves Part 2.
   */
  def eliminatedSize(polymer: Polymer): Int = {
    val reduced = reduce(polymer).toPolymer
    PTYPES.map { t =>
      reduce( reduced.filter(p => ptype(p) != t) ).toPolymer.length
    }.min
  }
  
  // MAIN //
  
  def main(args: Array[String]) = {
    val source = scala.io.Source.fromFile("input.txt")
    val content = try source.mkString finally source.close
    
    val polymer: Polymer = content.toList.map(PUnit)
    
    println( "Result of Part 1: " + size(polymer) )
    println( "Result of Part 2: " + eliminatedSize(polymer) )
    
    println("Bye!")
  }
  
}