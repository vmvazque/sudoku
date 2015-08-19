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
  var guessable = Set.empty[Cell]

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
    val missingCount = missing.count(!_.isFound())
    println("Missing: " + missingCount)

    for (c <- missing.filter(!_.isFound())) {
      c.setPossible(ALL_NUM -- holder.getTaken(c))  
      if (c.isFound()) {
        holder.addCell(c)
        hasChange = true
      }
    }

    val missingNow = missing.count(!_.isFound())
    println("Missing Now: " + missingNow)
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
      guessable = map(top.hashCode).updated(top.hashCode,top).values.map(_.snapshot()).toSet
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

  def setGuessableMap(c: Cell): Unit = {
    //right here i have a cell c and a Set[Cell]
    //what i want to do is store that in a map as
    //Map[c.hashCode, Map[each.hasCode, each]]
    
    //ok so this is turning a set to a Map[hashCode, Cell.snapshot]
    val sm = guessable.map(_.snapshot()).groupBy(_.hashCode()).mapValues(x => x.head)
    map = map + ((c.hashCode, sm))
    println("Stored cell: " + c.hashCode + " with guessable size: " + sm.size +
      " with guessed: " + sm.values.count(_.isGuessed))
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

  // def doGuessingPasses(isNew: Boolean): Unit = {
  //   // we need some sort of queue, so we can do something like
  //   // push(cell with 2) continue
  //   // is puzzle solved? cool
  //   // error? pop and push with 3
  //   // stuck? push the next one that needs to guess
  //   // step 1 get first missing that has not been found
  //   // step 2 possible should be set so take guess
  //   // step 3 push on queue 
  //   // doPasses
  //   // if invalid, undo all changes from last push and take next guess
  //   // if no next guess, undo all changes from push before that and so 
  //   // on, so wee nedd a way to keep track of changes
  //   //so visually say  we have 4 left, each can be (1,4,7)
  //   //so set guess n1 to be 1 in queue
  //   //try the rest did nothing
  //   //so set guess n2 to be 4 in queue (wed have to check the possibles again)
  //   //that changes 
  //   //n3 to be found
  //   //that breaks n4
  //   //now we have to go back, change n3 and pop it
  //   //change n2 
  //   //try again
    

  //   //get the first one
  //   //guess and push, 
  //   //now for every other one check the possible,
  //   //somehow recursively...
  //   //ok to add the base case:
  //   //if q is empty get first and guess, push on to q
  //   val m = missing.filter(!_.isFound)

  //   if (stack.isEmpty) {
  //     stack.push(m.head)
  //   }


  //   for (c <- m.tail) {
  //     c.setPossibleGuesses(ALL_NUM -- holder.getTaken(c) -- holder.getTaken(stack))
  //     if (!c.isValid) {
  //       stack.pop()
  //     }

  //   }
  //   //for remainder (setPossible ALL_NUM -- getTaken(c, q))
  //   //  if (invalid) {
  //   //    dq last change
  //   //  }
  //   //  if (found) {
  //   //    add to q
  //   //  }
  //   // }
  //   // if there was a change redo, if no change {
  //   //  dq last change and try again
  //   // }
  //   //this is the base case
  //   // for (c <- missing.filter(!_.isFound())) {
      
  //   // }
  //   // 
  //   // if isNew push to queue
  //   // isNew will be true on initial and on nothing new changed
    
  // }

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

    // var sb = new scala.collection.mutable.StringBuilder()
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

//
//i need like a stack map class which does the following:
//takes a Cell and a Set[Cell]
//on push pushes the cell WITH what the Set[Cell] looks like (snapshot)
//on call, return what the snapshot looked like?
//why is this betteR?
//i need to give you a copy of all the cells so you can manipulate them

// sealed class StackMap() {
//   //none of this should be necessary, just doing it to be sure hashcode and shit is
//   //working
//   private var stack = new Stack[Cell]
//   // private var cellMap = new Map.empty[Int, Cell]
//   private var map = new Map.empty[Int, Map[Int, Cell]]

//   //basically when i pop i want to either:
//   //return a List [Cells] how they looked when stored + however the top looks NOW
//   //so when i return a list of cells the cells must be duplicates because they
//   //will be changed
  
//   def push(c: Cell, s: Set[Cell]) = {
//     stack.push(c)
//     //todo: mapping
//   }

//   def pop(): Stack[Cell] = {
//     var top = stack.top()
//     //update top or pop it
//     //if pop do nothing
//     //if updated updated in map then return copies
//     //here we get a Set of what they looked like when they were added
//     //update
//     //what we want to do here is:
//     //if Cell on top has no more guesses, 
//     //reset to what it looked like
//   }
// }
// sealed class MyStack {
//   private var stack = new scala.collection.mutable.Stack[Cell]

//   def push(c: Cell) = stack.push(c)
//   def pop(): Unit = {
//     val last = stack.top
//     if (last.hasMoreGuesses) {
//       last.nextGuess()
//     } else {
//       last.reset()
//       stack.pop()
//       pop()
//     }
//   }

//   def isEmpty = stack.isEmpty
// }

sealed abstract class Holder() {
  val cols: Array[TreeSet[Cell]] = new Array[TreeSet[Cell]](9)
  val rows: Array[TreeSet[Cell]] = new Array[TreeSet[Cell]](9)
  val squares: Array[TreeSet[Cell]] = new Array[TreeSet[Cell]](9)
  
  reset()

  def reset() = {
    for (i <- 0 until 9) {
      cols(i) = new TreeSet[Cell]
      rows(i) = new TreeSet[Cell]
      squares(i) = new TreeSet[Cell]
    }  
  }
  

  private def isComplete(set: Set[Int]): Unit = {
    if (! ((Set(1,2,3,4,5,6,7,8,9) -- set).isEmpty) ) {
      throw new RuntimeException("WRONG");
    }
  }

  def validateAll(): Unit = {
    for (i <- 0 until 9) {
      isComplete(toNums(cols(i)))
      isComplete(toNums(rows(i)))
      isComplete(toNums(squares(i)))
    }
  }

  def getTaken(c: Cell) = {
    toNums(cols(c.col)) ++ toNums(rows(c.row)) ++ toNums(squares(getSquare(c)))
  }

  def toNums(s: TreeSet[Cell]) = getNums(s).map(_ match {
    case -1 => None
    case x => Some(x)
  }).flatten

  def getNums(s: TreeSet[Cell]): Set[Int]

  def addCell(c: Cell) = {
    cols(c.col) = cols(c.col) + c
    rows(c.row) = rows(c.row) + c
    squares(getSquare(c)) = squares(getSquare(c)) + c
  }

  def removeCell(c: Cell) = {
    cols(c.col) = cols(c.col) - c
    rows(c.row) = rows(c.row) - c
    squares(getSquare(c)) = squares(getSquare(c)) - c
  }

  private def getSquare(c: Cell) = {
    getRowSquare(c.row).intersect(getColSquare(c.col)).head
  }

  private def getRowSquare(row: Int) = {
    if (row < 3) {
      Set(0, 1, 2)
    } else if (row < 6) {
      Set(3,4,5)
    } else {
      Set(6,7,8)
    }
  }

  private def getColSquare(col: Int) = {
    if (col < 3) {
      Set(0,3,6)
    } else if (col < 6) {
      Set(1,4,7)
    } else {
      Set(2,5,8)
    }
  }

  def printNow(missing: Set[Cell]) = {
    val rowsNow = new Array[TreeSet[Cell]](9)
    for (i <- 0 until 9) {
      rowsNow(i) = rows(i).filter(_ => true)
    }

    for (c <- missing) {
      rowsNow(c.row) += c
    }

    for (i <- 0 until 9) {
      printRow(rowsNow(i))
    }
  }

  private def printRow(s: TreeSet[Cell]) = {
    for (c <- s) {
      print(c.getNum() match {
        case -1 => "?"
        case x => x
      })
    }
    println
  }
}

class CellHolder() extends Holder {
  def getNums(s: TreeSet[Cell]) = s.map(_.getNum())
}

class GuessHolder() extends Holder {
  def getNums(s: TreeSet[Cell]) = s.map(_.getGuess())
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


// 538 416 279
// 976 ?8? 541

// Lets look at that line, either one can be (2 or 3)
// so the logic we want is:
//   pick the first one, in which case its 2
//   continue the puzzle if any now becomes invalid go back and pick the next one
//   if none become invalid but not we have no more changes make the next change and
//   repeat



