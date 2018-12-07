package advent

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

import scala.util.parsing.combinator._
import advent.Day3._

object Test extends Properties("Main") {
  
  // GENERATORS //
  val genClaimId: Gen[ClaimId] = for {
    i <- Gen.choose(1,2000)
  } yield ClaimId(i)
  implicit val arbClaimId: Arbitrary[ClaimId] = Arbitrary(genClaimId)
  
  val genClaim: Gen[Claim] = for {
    id <- genClaimId
    cx <- Gen.choose(0,99)
    cy <- Gen.choose(0,99)
    ex <- Gen.choose(1,100-cx)
    ey <- Gen.choose(1,100-cy)
  } yield Claim( id, Corner(cx,cy), Extent(ex,ey) )
  implicit val arbClaim: Arbitrary[Claim] = Arbitrary(genClaim)
  
  // PROPERTIES //
  
  property("Claim.points.size is size of claim") = Prop.forAll { (x: Claim) =>
    x.points.size == x.extent._1 * x.extent._2
  }
  
  property("Partition.from.size is size of claim") = Prop.forAll { (x: Claim) => 
    Partition.from(x).toMap.keys.size == x.extent._1 * x.extent._2
  }
  
  property("Partition.from is stable under permutation") = Prop.forAll { (xs: List[Claim], perm: Int) =>
    val ys = xs.take(10)
    val zs = new scala.util.Random(perm).shuffle(ys)
    Partition.from(ys) == Partition.from(zs)
  }
  
  property("overlaps is zero for single claim") = Prop.forAll { (x: Claim) =>
    overlaps( List(x) ) == 0
  }
  
  property("overlaps is size for twice the same claim") = Prop.forAll { (x: Claim) =>
    val y = Claim(ClaimId(x.id + 1), x.corner, x.extent)
    val result = overlaps( List(x,y) )
    result == x.extent._1 * x.extent._2
  }
  
  property("noverlaps is identity for single claim") = Prop.forAll { (x: Claim) =>
    noverlaps( List(x) ) == Some(x)
  }
  
  property("noverlaps is empty for twice the same claim") = Prop.forAll { (x: Claim) =>
    val y = Claim(ClaimId(x.id + 1), x.corner, x.extent)
    noverlaps( List(x,y) ) == None
  }
  
  
  property("lineParser inverts toString") = Prop.forAll { (x: Claim) =>
    parse(lineParser, x.toString) match {
      case Success(matched,_) => matched == x
      case elze               => {
        println("match as " + elze)
        println("input as " + x)
        false
      }
    }
  }
  
}