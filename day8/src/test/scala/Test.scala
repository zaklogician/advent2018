package advent

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

import advent.Day8._

object Test extends Properties("Main") {
  
  // GENERATORS //
  val genEntry1: Gen[Entry] = for {
    metadata <- Gen.listOf(Gen.choose(0,5))
  } yield Entry(Nil, metadata.take(5))
  
  val genEntry2: Gen[Entry] = for {
    children <- Gen.listOf(genEntry1)
    metadata <- Gen.listOf(Gen.choose(0,5))
  } yield Entry(children,metadata.take(5))
  
  val genEntry: Gen[Entry] = for {
    children <- Gen.listOf(genEntry2)
    metadata <- Gen.listOf(Gen.choose(0,5))
  } yield Entry(children,metadata.take(5))
  implicit val arbEntry: Arbitrary[Entry] = Arbitrary(genEntry)
    
  // PROPERTIES //
  
  property("getEntry inverts toLicense") = Prop.forAll { (x: Entry) =>
    getEntry(x.toLicense) == x
  }
  
  property("Entry.value invariant under metadata permutation") = Prop.forAll { (x: Entry, perm: Int) =>
    val rng = new scala.util.Random(perm)
    val y = Entry(x.children, rng.shuffle(x.metadata))
    x.value == y.value
  }
  
}