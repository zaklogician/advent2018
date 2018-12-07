package advent

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

import scala.util.parsing.combinator._
import advent.Day1._

object Test extends Properties("Main") {
  
  // GENERATORS //
  
  val genFrequencyChange: Gen[FrequencyChange] = for {
    x <- Gen.choose(Int.MinValue, Int.MaxValue)
  } yield if (x != 0) FrequencyChange(x) else FrequencyChange(1)
  implicit val arbFrequencyChange: Arbitrary[FrequencyChange] =
    Arbitrary(genFrequencyChange)
  
  // PROPERTIES //
  
  property("partials is identity on empty list") = Prop.forAll { (x: Int) =>
    partials(Frequency(x), List()) == List( Frequency(x) )
  }
  
  property("partials is one longer than change list") = Prop.forAll { (x: Int, xs: List[FrequencyChange]) =>
    partials(Frequency(x), xs).length == xs.length + 1
  }
  
  property("partials of concatenation can be done in two passes") = Prop.forAll { (x: Int, xs: List[FrequencyChange], ys: List[FrequencyChange]) =>
    val first = Frequency(x)
    val second = partials(first, xs).last
    partials(first,xs).dropRight(1) ++ partials(second,ys) ==
    partials(first,xs ++ ys)
  }
  
  property("calibrate is zero on empty input") = Prop {
    calibrate( List() ) == Frequency(0)
  }
  
  property("calibrate is identity on singleton input") = Prop.forAll { (x: FrequencyChange) =>
    calibrate( List(x) ) == Frequency(x.toInt)
  }
  
  property("calibrate is stable under permutations") = Prop.forAll { (xs: List[FrequencyChange], perm: Int) =>
    val ys = new scala.util.Random(perm).shuffle(xs)
    calibrate( xs ) == calibrate( ys )
  }
  
  property("firstRepeated is NotFound on singleton input") = Prop.forAll { (x: FrequencyChange) =>
    !firstRepeated( List(x) ).found
  }
  
  property("firstRepeated is zero on positive-negative input") = Prop.forAll { (x: FrequencyChange) =>
    val p = x
    val n = FrequencyChange(0 - x.toInt)
    firstRepeated( List(p,n) ) == Found(Frequency(0))
  }
  
  property("lineParser inverts toString") = Prop.forAll { (x: FrequencyChange) =>
    parse(lineParser, x.toString) match {
      case Success(matched,_) => matched == x
      case _                  => false
    }
  }
  
}