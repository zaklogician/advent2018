package advent

import org.scalacheck._
import org.scalacheck.Properties
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._

import scala.util.parsing.combinator._
import advent.Day4._

object Test extends Properties("Main") {
  
  // GENERATORS //
  val genGuardId: Gen[GuardId] = for {
    i <- Gen.choose(1,9000)
  } yield GuardId(i)
  implicit val arbGuardId: Arbitrary[GuardId] = Arbitrary(genGuardId)
  
  val genEvent: Gen[Event] = for {
    id <- genGuardId
    tp <- Gen.oneOf(0,1,2)
  } yield tp match {
    case 0 => GuardBeginsShift(id)
    case 1 => WakesUp
    case 2 => FallsAsleep
  }
  implicit val arbEvent: Arbitrary[Event] = Arbitrary(genEvent)
  
  val genTimestamp: Gen[Timestamp] = for {
    yyyy <- Gen.choose(1000,2000)
    mm   <- Gen.choose(1,12)
    dd   <- Gen.choose(1,31)
    h <- Gen.choose(0,23)
    m <- Gen.choose(0,59)
  } yield Timestamp( yyyy + "-" + pad(mm.toString) + "-" + pad(dd.toString) + " " + pad(h.toString) + ":", Minute(m))
  implicit val arbTimestamp: Arbitrary[Timestamp] = Arbitrary(genTimestamp)
  
  val genLogEntry: Gen[LogEntry] = for {
    timestamp <- genTimestamp
    event <- genEvent
  } yield LogEntry(timestamp, event)
  implicit val arbLogEntry: Arbitrary[LogEntry] = Arbitrary(genLogEntry)
  
  val genMinute: Gen[Minute] = for {
    x <- Gen.choose(0,59)
  } yield Minute(x)
  implicit val arbMinute: Arbitrary[Minute] = Arbitrary(genMinute)
  
  val genSleepCount: Gen[SleepCount] = for {
    x <- Gen.choose(0,30)
  } yield SleepCount(x)
  implicit val arbSleepCount: Arbitrary[SleepCount] = Arbitrary(genSleepCount)
  
  val genSchedule: Gen[Schedule] = for {
    xs <- Gen.listOf(genMinute)
    ys <- Gen.listOf(genSleepCount)
  } yield Schedule { Map() ++ (xs,ys).zipped.toList }
  implicit val arbSchedule: Arbitrary[Schedule] = Arbitrary(genSchedule)
  
  // PROPERTIES //
  
  property("Schedule.+ has empty schedule as identity") = Prop.forAll { (x: Schedule) =>
    x + Schedule(Map()) == x &&
    x == x + Schedule(Map())
  }
  
  property("Schedule.+ is associative") = Prop.forAll { (x: Schedule, y: Schedule, z: Schedule) =>
    x + (y + z) == (x + y) + z
  }
  
  property("Schedule.update is idempotent") = Prop.forAll { (x: Schedule, y: Schedule) =>
    ((x update y) update y) == (x update y)
  }
  
  property("Schedule.update on FallAsleep() works as intended") = Prop.forAll { (x:Schedule, m: Minute) =>
    val y = x update FallAsleep(m)
    minutes.forall { k =>
      (k < m) || y.toMap(k) == 1
    }
  }
  
  property("Schedule.update on WakeUp() works as intended") = Prop.forAll { (x:Schedule, m: Minute) =>
    val y = x update WakeUp(m)
    minutes.forall { k =>
      (k < m) || y.toMap(k) == 0
    }
  }
  
  
  
  property("logEntryParser inverts toString") = Prop.forAll { (x: LogEntry) =>
    parse(logEntryParser, x.toString) match {
      case Success(matched,_) => matched == x || { println(matched);println(x);false}
      case elze               => false
    }
  }
  
}