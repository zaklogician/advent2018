package advent

import scala.collection.generic.CanBuildFrom
import scala.util.parsing.combinator._

import shapeless._
import shapeless.tag.@@
import newtype._



object Day1 extends RegexParsers {
  
  // PROBLEM DOMAIN //
  
  trait FrequencyTag {}
  type Frequency = Int @@ FrequencyTag
  def Frequency(x: Int): Frequency = tag[FrequencyTag][Int](x)
  
  case class FrequencyChange(toInt: Int) {
    override def toString: String =
      if (toInt <= 0) toInt.toString else ("+" + toInt.toString)
    def apply(target: Frequency): Frequency = Frequency(target+this.toInt)
  }
  object FrequencyChange {
    def between(start: Frequency, end: Frequency) = FrequencyChange(end - start)
  }
  
  
  /**
   * Returns the list of frequencies the device reaches over a list of changes.
   */
  def partials[C <: Traversable[FrequencyChange], That](start: Frequency, changes: C)
    (implicit bf: CanBuildFrom[C, Frequency, That]) : That = {
    val b = bf()
    b ++= changes.scanLeft(start) { (freq, change) => change(freq) }
    b.result
  } 
  
  
  /**
   * Returns the frequency the device reaches after all the frequency changes in `changes`
   * have been applied, starting with a frequency of zero. Solves Part 1.
   */
  def calibrate(changes: Traversable[FrequencyChange]): Frequency =
    partials(Frequency(0), changes).last
  
  
  /**
   * Represents the state of the operation that searches for a repeating frequency.
   */
  sealed trait RepetitionResult {
    def update(freq: Frequency): RepetitionResult
    def found: Boolean
  }
  case class NotFound(seen: Set[Frequency]) extends RepetitionResult {
    def update(frequency: Frequency): RepetitionResult = 
      if (seen contains frequency) Found(frequency)
      else NotFound(seen + frequency)
    def found: Boolean = false
  }
  case class Found(frequency: Frequency) extends RepetitionResult {
    def update(freq: Frequency): RepetitionResult = this
    def found: Boolean = true
  }
  
  /**
   * Finds the first frequency my device reaches twice on the provided list of changes.
   * Returns `Found(x)` if `x` is the first repeated frequency, and `NotFound` if 
   * the given list of frequencies never repeats.
   * 
   * Note: If a repeated value is not found under `max-min` repetitions, where
   * `max` is the maximum and `min` is the minimum frequency reached over a single
   * repetition, then no repeated value can exist. This is because all the frequencies
   * drift by the calibration frequency (see `calibrate`) over multiple repetitions.
   */
  def firstRepeated(changes: Traversable[FrequencyChange]): RepetitionResult = {
    val p: Traversable[Frequency] = partials( Frequency(0), changes)
    val bound = (p.maxBy(_.toInt) - p.minBy(_.toInt)) / (1 + Math.abs(p.last))
    val looped: Stream[Frequency] = partials( Frequency(0), Stream.continually(changes).take(bound).flatten )
    looped.scanLeft[RepetitionResult,Stream[RepetitionResult]]( NotFound(Set()) ) {
      (state, freq) => state.update(freq)
    }.dropWhile( !_.found ).headOption.getOrElse( NotFound(Set()) )
  }
  
  // INPUT HANDLING //
  
  def lineParser: Parser[FrequencyChange] =
    """(\+[1-9]\d*|\-[1-9]\d*)""".r ^^ { x => FrequencyChange(x.toInt) }
  
  def inputParser: Parser[List[FrequencyChange]] = rep(lineParser)
  
  
  // MAIN //
  def main(args: Array[String]) = {
    val source = scala.io.Source.fromFile("input.txt")
    val content = try source.mkString finally source.close
    
    parse(inputParser, content) match {
      case Failure(msg,_)     => println("Parsing failed: " + msg)
      case Error(msg,_)       => println("Error: " + msg)
      case Success(matched,_) => {
        val part1 = calibrate(matched)
        println("Result of Part 1: " + part1)
        
        val part2 = firstRepeated(matched)
        println("Result of Part 2: " + part2)
      }
    }
    
    
    println("Bye!")
  }
  
}