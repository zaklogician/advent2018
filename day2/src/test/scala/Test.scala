package advent

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

import advent.Day2._

object Test extends Properties("Main") {
  
  // GENERATORS //
  
  val genAlphabetical: Gen[Char] = Gen.oneOf(ALPHABET)
  
  val genBoxID: Gen[BoxID] = for {
    xs <- Gen.listOf( genAlphabetical )
  } yield BoxID(xs.mkString)
  implicit val arbBoxID: Arbitrary[BoxID] =
    Arbitrary(genBoxID)
  
  val genCharCount: Gen[CharCount] = for {
    id <- genBoxID
  } yield id.charCount
  implicit val arbCharCount: Arbitrary[CharCount] =
    Arbitrary(genCharCount)
  
  
  // PROPERTIES //
  
  property("CharCount.equals is reflexive") = Prop.forAll { (x: CharCount) =>
    x == x
  }
  
  property("CharCount.empty is identity element for +") = Prop.forAll { (x: CharCount) =>
    (x + CharCount.empty) == x
  }
  
  property("CharCount.+ is commutative") = Prop.forAll { (x: CharCount, y: CharCount) =>
    (x + y) == (y + x)
  }
  
  property("CharCount.+ is associative") = Prop.forAll { (x: CharCount, y: CharCount, z: CharCount) =>
    (x + y) + z == x + (y + z)
  }
  
  property("BoxID.neighbors contains original element") = Prop.forAll { (x:BoxID) =>
    x.asString.isEmpty || x.neighbors.contains(x)
  }
  
  property("BoxID.charcount of empty is empty") = Prop {
    BoxID("").charCount == CharCount.empty
  }
  
  property("BoxID.charcount is monoid homomorphism") = Prop.forAll { (x: BoxID, y: BoxID) =>
    BoxID(x.asString + y.asString).charCount == x.charCount + y.charCount
  }
  
  property("BoxID.charcount has total equal to length") = Prop.forAll { (x: BoxID) =>
    x.charCount.toList.sum == x.asString.length
  }
  
  property("BoxID.charcount counts the number of occurrences") = Prop.forAll { (x: BoxID, c: Char) =>
    x.charCount.toMap.getOrElse(c,0) == x.asString.count(_ == c)
  }
  
  property("checksum is stable under permutations") = Prop.forAll { (xs: List[BoxID], perm: Int) =>
    val ys = new scala.util.Random(perm).shuffle(xs)
    checksum(xs) == checksum(ys)
  }
  
  property("checksum of empty list is 0") = Prop {
    checksum( List() ) == 0
  }
  
  property("checksum is bounded above by length square") = Prop.forAll { (xs: List[BoxID]) =>
    checksum(xs) <= xs.length*xs.length
  }

}