package scala.sudoku

import scala.collection.immutable._

class Solver(puzzle: String) {
  val ALL_NUM = Set(1,2,3,4,5,6,7,8,9)

  val split = """\r?\n""".r
  val holder = new CellHolder()
  var missing = List.empty[Cell]

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

  def doGuessingPasses(isNew: Boolean): Unit = {
    // we need some sort of queue, so we can do something like
    // push(cell with 2) continue
    // is puzzle solved? cool
    // error? pop and push with 3
    // stuck? push the next one that needs to guess
    // step 1 get first missing that has not been found
    // step 2 possible should be set so take guess
    // step 3 push on queue 
    // doPasses
    // if invalid, undo all changes from last push and take next guess
    // if no next guess, undo all changes from push before that and so 
    // on, so wee nedd a way to keep track of changes
    //so visually say  we have 4 left, each can be (1,4,7)
    //so set guess n1 to be 1 in queue
    //try the rest did nothing
    //so set guess n2 to be 4 in queue (wed have to check the possibles again)
    //that changes 
    //n3 to be found
    //that breaks n4
    //now we have to go back, change n3 and pop it
    //change n2 
    //try again
    
    //get the first one
    //guess and push, 
    //now for every other one check the possible,
    //somehow recursively...
    //ok to add the base case:
    //if q is empty get first and guess, push on to q
    //for remainder (setPossible ALL_NUM -- getTaken(c, q))
    //  if (invalid) {
    //    dq last change
    //  }
    //  if (found) {
    //    add to q
    //  }
    // }
    // if there was a change redo, if no change {
    //  dq last change and try again
    // }
    //this is the base case
    // for (c <- missing.filter(!_.isFound())) {
      
    // }
    // 
    // if isNew push to queue
    // isNew will be true on initial and on nothing new changed
    
  }

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
          missing = missing:+ new Cell(row, i)
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

sealed class MyStack {
  private var stack = new scala.collection.mutable.Stack[Cell]

  def push(c: Cell) = stack.push(c)
  def pop() = {
    val last = stack.top
    if (last.hasMoreGuesses)
  }


}

class CellHolder() {
  val cols: Array[TreeSet[Cell]] = new Array[TreeSet[Cell]](9)
  val rows: Array[TreeSet[Cell]] = new Array[TreeSet[Cell]](9)
  val squares: Array[TreeSet[Cell]] = new Array[TreeSet[Cell]](9)

  for (i <- 0 until 9) {
    cols(i) = new TreeSet[Cell]
    rows(i) = new TreeSet[Cell]
    squares(i) = new TreeSet[Cell]
  }

  def getTaken(c: Cell) = {
    toNums(cols(c.col)) ++ toNums(rows(c.row)) ++ toNums(squares(getSquare(c)))
  }

  private def toNums(s: TreeSet[Cell]) = s.map(_.getNum() match {
    case -1 => None
    case x => Some(x)
  }).flatten

  def addCell(c: Cell) = {
    cols(c.col) = cols(c.col) + c
    rows(c.row) = rows(c.row) + c
    squares(getSquare(c)) = squares(getSquare(c)) + c
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

  def printNow(missing: List[Cell]) = {
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
    |538 ?16 ?7?
    |??? ?8? 541
    |241 5?? ???

    |?6? 9?? ???
    |??? ?35 ?9?
    |?9? ??4 ???

    |6?? ??? 93?
    |129 ?4? ?5?
    |??4 69? ??8
    """.stripMargin

  val solver = new Solver(easyish)

  solver.buildPuzzle()
  solver.doPasses()
  solver.printRows


}


538 416 279
976 ?8? 541

Lets look at that line, either one can be (2 or 3)
so the logic we want is:
  pick the first one, in which case its 2
  continue the puzzle if any now becomes invalid go back and pick the next one
  if none become invalid but not we have no more changes make the next change and
  repeat



