package advent

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

import advent.Day5._

object Test extends Properties("Main") {
  
  // GENERATORS //
  
  val genPUnit: Gen[PUnit] = for {
    i <- Gen.oneOf(PTYPES)
    f <- Gen.oneOf( (x:PType) => PUnit(x.toUpper), (x: PType) => PUnit(x.toLower) )
  } yield f(i)
  implicit val arbPUnit: Arbitrary[PUnit] = Arbitrary(genPUnit)
  
  val genReduced: Gen[Reduced] = for {
    p <- Gen.listOf(genPUnit)
  } yield reduce(p)
  implicit val arbReduced: Arbitrary[Reduced] = Arbitrary(genReduced)
  
  // PROPERTIES //
  
  property("polarity and .ptype determine PUnit") = Prop.forAll { (x: PUnit, y: PUnit) =>
    ( ptype(x) == ptype(y) && polarity(x) == polarity(y) ) == (x == y)
  }
  
  /** The invariant of having no reactive pairs of `PUnit`s. */
  def invariant(reduced: Reduced): Boolean = {
    val polymer = reduced.toPolymer
    polymer.sliding(2,1).forall { x =>
      (x.length < 2) || !reactive(x.head,x.tail.head)
    }
  }
  
  property("Reduced.+ maintains Reduced invariant") = Prop.forAll { (r: Reduced, x: PUnit) =>
    invariant( r + x )
  }
  
  property("react maintains Reduced invariant") = Prop.forAll { (r: Reduced, p: Polymer) =>
    invariant(  react(r,p)  )
  }
  
  property("reduce on reduced is identity") = Prop.forAll { (r: Reduced) =>
    reduce( r.toPolymer ) == r 
  }
  
  property("reduce shortens Polymer") = Prop.forAll { (p:Polymer) =>
    reduce(p).toPolymer.length <= p.length
  }
  
}