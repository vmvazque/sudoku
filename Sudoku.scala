package scala.sudoku

import scala.collection.immutable._
import scala.collection.mutable.Stack

class Solver(puzzle: String) {
  val ALL_NUM = Set(1,2,3,4,5,6,7,8,9)

  val split = """\r?\n""".r
  val holder = new CellHolder()
  val guessHolder = new GuessHolder()
  var missing = Set.empty[Cell]

  var map = Map.empty[Int, Map[Int, Cell]]
  // var map = Map.empty[Cell, Set[Cell]]
  var guessable = Set.empty[Cell]
  //keep a map of which values for cells cause errors so we dont check them again
  var errMap = Map.empty[Int, Set[Int]]

  def buildPuzzle() =  {
    var i = 0
    for (s <- split.split(puzzle)) {
      if (!s.trim.isEmpty()) {
        parseLine(s,i)
        i+=1
      }
    }
  }

  def doPasses(): Unit = {
    var hasChange = false
    for (c <- missing.filter(!_.isFound())) {
      c.setPossible(ALL_NUM -- holder.getTaken(c))
      if (c.isFound()) {
        hasChange = true
      }
    }

    if (hasChange) {
      doPasses()
    }
  }

  def initGuessable() = {
    guessable = missing.filter(!_.isFound())
    guessable.foreach(_.resetGuesses)
  }

  def solve(): Unit = {
    doPasses()
    initGuessable()
    if (guessable.size > 0) {
      doGuesses(new Stack[Cell](), true) 
      holder.validateAll() 
    } else {
      printRows()
      holder.validateAll()
    }
  }

  def printComplete() = {
    println("Num without guessed: " + guessable.count(!_.isGuessed))
    guessable.foreach(x => {
      x.markComplete()
      holder.addCell(x)
    })

    holder.printNow(Set.empty[Cell])
  }

  private def advanceGuess(stack: Stack[Cell]): Unit = {
    if (stack.isEmpty) {
      println("Uhhh...no more guesses, idk why")
    }

    val top = stack.top
    //reset missing
    // println("Resetting guessable")
    //map is [Int, Map[Int, Cell]] want map(int)
    //map(int) gets me a Map[Int, Cell] m1
    //m1.updated((top.hashCode, top)).values
    // guessable = map(top)
    if (top.hasMoreGuesses) {
      println("Has more guesses, advancing")
      top.nextGuess()
      if (!isValidGuess(top)) {
        advanceGuess(stack)
      }
      guessable = map(top.hashCode).updated(top.hashCode,top).values.map(_.snapshot()).toSet
      // guessable = (map(top) + top).map(_.snapshot())
      //the problem right now is this sets guessable to be a set of things then
      //when you need to reset it without popping it, you want to go back to what it was
      //so basically:
      //update Cells in guessable
      //we need to reset? ok give me the old stuff that was there when i first set it
      //
      println("reset cell: " + top.hashCode + " with guessable size: " + guessable.size +
        " with guessed: " + guessable.count(_.isGuessed))
      // println("Missing now: " + guessable.size)
      // println("Unguessed now: " + guessable.count(!_.isGuessed))
      // guessable = guessable + top
      guessHolder.reset()
      guessable.filter(_.isGuessed).foreach(guessHolder.addCell(_))
      // top.printGuess()
    } else {
      println("No more guesses poping this thing")
      // top.resetGuesses()
      stack.pop()
      // println("Stack size: " + stack.size)
      advanceGuess(stack)
    }
  }

  private def isValidGuess(c: Cell): Boolean = {
    !errMap.getOrElse(c.hashCode(), Set.empty[Int]).contains(c.getGuess)
  }

  def setGuessableMap(c: Cell): Unit = {
    //right here i have a cell c and a Set[Cell]
    //what i want to do is store that in a map as
    //Map[c.hashCode, Map[each.hasCode, each]]
    
    //ok so this is turning a set to a Map[hashCode, Cell.snapshot]
    val sm = guessable.map(_.snapshot()).groupBy(_.hashCode()).mapValues(x => x.head)
    map = map + ((c.hashCode, sm))
    println("Stored cell: " + c.hashCode + " with guessable size: " + sm.size +
      " with guessed: " + sm.values.count(_.isGuessed))
    // map = map.updated(c, guessable.map(_.snapshot()))
  }

  def doGuesses(stack: Stack[Cell], add: Boolean): Unit = {
    if (add) {
      val c = guessable.filter(!_.isGuessed).head
      stack.push(c)
      //map is [Int, Map[Int, Cell]]
      // map = map + (( c.hashCode, guessable.map(_.snapshot()) ))
      setGuessableMap(c)
      // map = map + ((c, guessable.map(_.snapshot())))
      // println("added snapshot of missing count: " + map.size)
      println("Pushed something, now calling advance")
      advanceGuess(stack)
    }

    if (checkGuess()) {
      println("Checking guess returned true")
      if (isComplete()) {
        println("Done with this shit")
        printComplete()
      } else {
        println("Not complete, doing guesses again")
        doGuesses(stack, true)
      }
    } else {
      println("Guess failed, trying again")
      addFailed(stack)
      //the problem is around here somewhere
      //basically we dont want to undo ALL changes
      //so if n1 changes n3 but doesnt set it, then n2 changes
      //n3 but doesnt set it, then we want to reset n3 to pre n2 when n2 changes
      //and change it again when n1 changes, not whenever it fails...
      //so we have to keep track of everything untill then so you want a snapshot
      //whenever we push onto the stack we store all the cells including this one
      //as a snapshot with some sort of counter, whenever we pop we reset everything 
      //back to the snapshot version...and then reset everything we need to do
      //but how do we store snapshots and reset them? basically store this stack
      //with the cell and the restore it?
      // (missing -- stack).foreach(x=> {
      //   x.resetGuesses()
      //   guessHolder.removeCell(x)
      // })
      advanceGuess(stack)
      doGuesses(stack, false)
    }
  }

  private def addFailed(stack: Stack[Cell]) = {
    //do nothing cause that makes no sense at all, im dumb
    // val top = stack.top
    // var errors = errMap.getOrElse(top.hashCode(), Set.empty[Int])
    // errMap = errMap.updated(top.hashCode(), errors + top.getGuess())
  }

  private def checkGuess(): Boolean = {
    var change = false
    for (c <- guessable.filter(!_.isGuessed)) {
      c.setPossibleGuesses(ALL_NUM -- holder.getTaken(c) -- guessHolder.getTaken(c))
      // c.setPossibleGuesses(???)
      if (c.isGuessed) {
        change = true
        guessHolder.addCell(c)
      } else if (!c.isValid) {
        println("Not valid found")
        return false
      }
    }

    if (change) {
      println("Something changed, lets check again")
      println("Currently guessed right now: " + guessable.count(_.isGuessed))
      checkGuess()
    } else {
      println("Hmm, that didnt change anything, but didnt break")
      println("Currently guessed right now: " + guessable.count(_.isGuessed))
      true
    }
  }

  private def isComplete() = guessable.filter(!_.isGuessed).isEmpty

  def printRows() = {
    holder.printNow(missing.filter(!_.isFound()))
  }

  private def parseLine(s: String, row: Int) = {
    def parseChar(c: Char) = {
      c match {
        case '?' => Some(0)
        case x if (x != ' ') => Some(Integer.parseInt(""+x))
        case _ => None
      }
    }

    var i = -1
    for (c <- s.trim.toCharArray()) {
      i+=1
      parseChar(c) match {
        case Some(x) if (x == 0) => {
          missing = missing + new Cell(row, i)
        }
        case Some(x) => {
          holder.addCell(new Cell(x, row, i))
        } 
        case _ => {
          i-=1
        }
      }
    }
  }
}


object main extends App {
  println("Hello World")
  val medium = """
  |1????5??4
  |?234?6?5?
  |???????6?
  |9?4????7?
  |???567???
  |?5????8?3
  |?4???????
  |?3?7?198?
  |2??8????7
  """.stripMargin

  val easy = """
    |538 ?16 ?79
    |??? 38? 541
    |241 5?? ???

    |?6? 9?? ???
    |??? ?35 ?9?
    |?9? ??4 ??2

    |6?? 2?? 93?
    |129 ?4? ?5?
    |?54 69? ??8
    """.stripMargin

  val easyish = """
    |538 ?16 ?79
    |??? 38? 541
    |241 5?? ???

    |?6? 9?? ???
    |??? ?35 ?9?
    |?9? ??4 ??2

    |6?? 2?? 93?
    |129 ?4? ?5?
    |?54 6?? ??8
    """.stripMargin

  val hard = """
  |???84???9
  |??1?????5
  |8???2146?
  |7?8????9?
  |?????????
  |?5????3?1
  |?2491???7
  |9?????5??
  |3???84???
  """.stripMargin

  var extreme = """
  |8?? ??? ???
  |??3 6?? ???
  |?7? ?9? 2??
  |?5? ??7 ???
  |??? ?45 7??
  |??? 1?? ?3?
  |??1 ??? ?68
  |??8 5?? ?1?
  |?9? ??? 4?? 
  """.stripMargin
  // val solver = new Solver(easyish)

  // solver.buildPuzzle()
  // solver.solve()

  // println("Without guessing")
  // val s = new Solver(easy)
  // s.buildPuzzle()
  // s.solve()

  // println("Hard")
  // val m = new Solver(hard)
  // m.buildPuzzle()
  // m.solve()

  println("Medium")
  val me = new Solver(medium)
  me.buildPuzzle()
  me.solve()
  // solver.buildPuzzle()
  // solver.doPasses()
  // solver.printRows
  // 
  // println("EXTREME!!")
  // val e = new Solver(extreme)
  // e.buildPuzzle()
  // e.solve()


}
