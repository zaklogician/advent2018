package advent

import scala.collection.SortedMap
import scala.util.parsing.combinator._

import shapeless.tag
import shapeless.tag.@@

object Day4 extends RegexParsers {
    
  // PROBLEM DOMAIN: MAIN ABSTRACTIONS //
  
  trait GuardIdTag {}
  type GuardId = Int @@ GuardIdTag
  def GuardId(x: Int): GuardId = tag[GuardIdTag][Int](x)
  
  trait MinuteTag {}
  type Minute = Int @@ MinuteTag
  def Minute(x: Int): Minute = tag[MinuteTag][Int](x % 60)
  
  trait SleepCountTag {}
  type SleepCount = Int @@ SleepCountTag
  def SleepCount(x: Int): SleepCount = tag[SleepCountTag][Int](x)
  
  val minutes: Seq[Minute] = (0 to 59).map(Minute)
  
  /**
   * A `Schedule` is a partial function that associates the 
   * number of days when the owner spent a given `Minute`
   * asleep.
   * 
   * Note: `Schedule` forms a monoid the obvious way, but we can
   * introduce another operation, `update`, which allows us to 
   * describe how two consecutive `Event`s compose to affect the
   * schedule.
   */
  case class Schedule(toMap: Map[Minute, SleepCount]) {
    /**
     * Overwrites all points of this `Schedule` with
     * points of the given schedule. Leaves points that
     * are not defined in the given `Schedule` unchanged.
     */
    def update(next: Schedule): Schedule = Schedule {
      this.toMap ++ next.toMap.toList
    }
    
    def +(next: Schedule): Schedule = Schedule {
      this.toMap ++ next.toMap.keys.map { k => k -> (
        SleepCount( this.toMap.getOrElse(k,0) + next.toMap(k) )
      )}
    }
    
    /** The total number of minutes the owner of this `Schedule spent asleep. */
    lazy val total: Int = toMap.values.map(_.toInt).sum
    
    /** The minute when the owner of this `Schedule` is most likely to be asleep. */
    lazy val bestMinute: Minute = toMap.maxBy(x => x._2.toInt)._1
  }
  object AwakeSchedule extends Schedule( Map() ++ minutes.map(k => k -> SleepCount(0)) )
  def WakeUp(start: Minute): Schedule = Schedule {
    Map() ++ (for {m <- minutes; if (m >= start)} yield (m -> SleepCount(0)))
  }
  def FallAsleep(start: Minute): Schedule = Schedule {
    Map() ++ (for {m <- minutes; if (m >= start)} yield (m -> SleepCount(1)))
  }
  
  /**
   * A `Behavior` associates a `Schedule` to each guard.
   * 
   * Note: forms a monoid the obvious way.
   */
  case class Behavior(toMap: Map[GuardId,Schedule]) {
    def +(guard: GuardId, next: Schedule): Behavior = Behavior {
      val current: Schedule = toMap.getOrElse(guard, AwakeSchedule)
      toMap + ( guard -> (current + next) )
    }
    def bestSleeper: GuardId = toMap.maxBy( x => x._2.total )._1
  }
  
  /**
   * Finds the `Behavior` associated to the input `Log`.
   */
  def process(input: Log): Behavior = {
    val sorted = input.sortBy(_.timestamp.toString).toList
    def impl( guard: GuardId, behavior: Behavior, schedule: Schedule, xs: List[LogEntry]): Behavior = xs match {
      case LogEntry(t, WakesUp) :: xs =>
        impl(guard, behavior, schedule update WakeUp(t.minute), xs)
      case LogEntry(t, FallsAsleep) :: xs =>
        impl(guard, behavior, schedule update FallAsleep(t.minute), xs)
      case LogEntry(_, GuardBeginsShift(newGuard)) :: xs => 
        impl(newGuard,behavior + (guard, schedule), AwakeSchedule, xs)
      case Nil => behavior + (guard, schedule)
    }
    impl( GuardId(-1), Behavior( Map() ), AwakeSchedule, sorted)
  }
  
  /**
   * Finds the product of
   * 1. The `GuardID` of the guard that has the most minutes asleep 
   *    in the given `Behavior`.
   * 2. The `Minute` in which said guard is most likely to be asleep.
   * Solves Part 1.
   */
  def mostMinutesAsleep(behavior: Behavior): Int = {
    val bestSleeper = behavior.bestSleeper
    bestSleeper * behavior.toMap(bestSleeper).bestMinute
  }
  
  /**
   * Finds the product of the `Minute` and the `GuardID` of the guard 
   * maximizing number of days asleep on said `Minute`. Solves Part 2.
   */
  def bestCombination(behavior: Behavior): Int = {
    val guards = behavior.toMap.keys.filter(_ >= 0)
    val gm = for {
      g <- guards
      m <- minutes
    } yield (g,m)
    val result = gm.maxBy { k => 
      behavior.toMap(k._1).toMap.getOrElse(k._2,SleepCount(0)).toInt
    }
    result._1 * result._2
  }


  
  // PROBLEM DOMAIN: INPUT ABSTRACTIONS //
  
  sealed trait Event { }
  case class GuardBeginsShift(guardId: GuardId) extends Event {
    override def toString: String = "Guard #" + guardId + " begins shift"
  }
  case object WakesUp extends Event {
    override def toString: String = "wakes up"
  }
  case object FallsAsleep extends Event {
    override def toString: String = "falls asleep"
  }
  
  def pad(xs: String) = xs.reverse.padTo(2,'0').reverse
  
  case class Timestamp(etc: String, minute: Minute) {
    override def toString: String = "[" + etc + pad(minute.toString) + "]"
  }
  
  case class LogEntry(timestamp: Timestamp, event: Event) {
    override def toString: String = timestamp.toString + " " + event
  }
  
  type Log = Seq[LogEntry]
  
  
  // INPUT HANDLING //
  
  val guardIdParser: Parser[GuardId] = 
    """#[1-9]\d*""".r ^^ { case x => GuardId(x.tail.toInt) } 
  
  val dumbEventParser: Parser[Event] =
    """(falls asleep|wakes up)""".r ^^ { case x => 
      if (x == "falls asleep") FallsAsleep else WakesUp
    }
  
  val guardEventParser: Parser[Event] = 
    ("""Guard""".r ~ guardIdParser ~ "begins shift") ^^ {
      case (_ ~ x ~ _) => GuardBeginsShift(x) 
    }
    
  val eventParser: Parser[Event] =
    (dumbEventParser | guardEventParser) ^^ {
      case x => x
    }
  
  val etcParser: Parser[String] = 
    """\d*\-\d\d-\d\d \d\d:""".r ^^ {
      case d => d
    }

  val minuteParser: Parser[Minute] = 
    """\d\d""".r ^^ { case mm => Minute(mm.toInt) }

  val timestampParser: Parser[Timestamp] = 
    (literal("[") ~> etcParser ~ minuteParser <~ literal("]")) ^^ {
      case (etc ~ minute) => Timestamp(etc,minute)
    }

  val logEntryParser: Parser[LogEntry] =
    (timestampParser ~ eventParser) ^^ {
      case (timestamp ~ event) => LogEntry(timestamp,event)
    }
  
  def inputParser: Parser[List[LogEntry]] = rep(logEntryParser)
  
  // MAIN //
  
  def main(args: Array[String]) = {
    val source = scala.io.Source.fromFile("input.txt")
    val content = try source.mkString finally source.close
    
    parse(inputParser, content) match {
      case Failure(msg,_)     => println("Parsing failed: " + msg)
      case Error(msg,_)       => println("Error: " + msg)
      case Success(matched,_) => {
        val behavior: Behavior = process(matched)
        println("Result of Part 1: " + mostMinutesAsleep(behavior))
        println("Result of Part 2: " + bestCombination(behavior))
      }
    }
    println("Bye!")
  }
  
}