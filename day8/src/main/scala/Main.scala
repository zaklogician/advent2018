package advent

object Day8 {
    
  // PROBLEM DOMAIN //
  
  type License = List[Int]
  
  sealed trait Rest[A] {
    def rest: License
  }
  case class Failure[A](error: String, rest: License) extends Rest[A]
  case class Success[A](value: A, rest: License) extends Rest[A]
  
  /**
   * A monad for parsing a list of integers.
   */
  case class Consumer[A](runConsumer: License => Rest[A]) {
    def flatMap[B]( f: A => Consumer[B] ): Consumer[B] = Consumer[B] { file =>
      this.runConsumer(file) match {
        case Success(value, rest) => f(value).runConsumer(rest)
        case Failure(error, rest) => Failure(error, rest)
      }
    }
    def map[B]( f: A => B ): Consumer[B] = flatMap(Consumer unit f(_))    
    def apply(file: License): A = runConsumer(file) match {
      case Success(value, rest) => value
      case Failure(error, rest) => throw new Exception(error)
    }
  }
  object Consumer {
    def unit[A](a: A) = Consumer(file => Success(a,file))
  }

  case class Entry(children: List[Entry], metadata: List[Int]) {
    def toLicense: License = {
      children.length :: metadata.length :: (children.flatMap(_.toLicense) ++ metadata)
    }

    /** 
     * Returns the number obtained by recursively adding up all the
     * metadata entries. Solves Part 1.
     */
    def checksum: Int = children.map(_.checksum).sum + metadata.sum
    
    /**
     * Returns the value of this node. Solves Part 2.
     */
    def value: Int = if (children.isEmpty) checksum else {
      val subvalues = children.map(_.value)
      val metavalues = for {
        m <- metadata
        if (m > 0)
        if (m <= subvalues.length)
      } yield subvalues(m-1)
      metavalues.sum
    }
  }
  
  def getHeader: Consumer[(Int,Int)] = Consumer { file => file match {
    case (children :: metadata :: rest) => Success((children,metadata),rest)
    case _ => Failure("Underflow: header needs 2 entries.", file)
  }}
  
  def getChildren(children: Int): Consumer[List[Entry]] =
    if (children <= 0) Consumer.unit(Nil) else for {
      h <- getEntry
      t <- getChildren(children - 1)
    } yield (h :: t)
    
  def getMetadata(metadata: Int): Consumer[List[Int]] = Consumer { file => 
    val content = file.take(metadata)
    val rest = file.drop(metadata)
    if (content.length == metadata) Success(content,rest)
    else Failure("Underflow: metadata needs " + metadata + " entries.", file)
  }
  
  def getEntry: Consumer[Entry] = for {
    header <- getHeader
    children <- getChildren(header._1)
    metadata <- getMetadata(header._2)
  } yield Entry(children,metadata)
  
  // INPUT HANDLING //
  
  // MAIN //
  
  def main(args: Array[String]) = {
    val source = scala.io.Source.fromFile("input.txt")
    val content = try source.mkString finally source.close
    val file: License = content.split(" ").toList.map(_.toInt)
    
    val entry: Entry = getEntry(file)
    println("Result of Part 1: " + entry.checksum)
    println("Result of Part 2: " + entry.value)
    println("Bye!")
  }
  
}