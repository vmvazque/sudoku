package scala.sudoku
import scala.math.Ordered.orderingToOrdered

object Cell {
  def getRowSquare(row: Int) = {
    if (row < 3) {
      Set(0, 1, 2)
    } else if (row < 6) {
      Set(3,4,5)
    } else {
      Set(6,7,8)
    }
  }


  def getColSquare(col: Int) = {
    if (col < 3) {
      Set(0,3,6)
    } else if (col < 6) {
      Set(1,4,7)
    } else {
      Set(2,5,8)
    }
  }

}

class Cell(val row: Int,val  col: Int) extends Ordered[Cell] {
  private var possible: Set[Int] = Set.empty[Int]
  private var guesses: Set[Int] = Set.empty[Int]

  private var num: Int = -1
  private var guess: Int = -1

  def this(initNum: Int, row: Int, col: Int) {
    this(row, col);
    num = initNum
  }  

  lazy val square = Cell.getRowSquare(row).intersect(Cell.getColSquare(col)).head

  def setPossible(pos: Set[Int]) = {
    possible = pos
    possible.size match {
      case 0 => throw new RuntimeException("Not possible to have no possible")
      case 1 => num = possible.head
      case _ => ;
    }
  }

  def setPossibleGuesses(pos: Set[Int]) = {
    guesses = pos
    if (guesses.size == 1) {
      guess = guesses.head
    }
  }

  def isFound() = num > 0

  def getNum() = num
  
  def compare(that: Cell): Int = (this.row, this.col) compare (that.row, that.col)

  def isValid = guesses.size > 0
  def isGuessed = guess > 0

  def resetGuesses() = {
    guesses = Set(possible.toList:_*)
    guess = -1
  }

  def hasMoreGuesses = guesses.size > 0 

  def nextGuess() = {
    guess = guesses.head
    guesses = guesses.tail
  }

  def getGuess() = guess

  def printGuess() = {
    println("Current Guess: " + guess)
    println("Remaining Guesses: " + guesses)
    println("Is Guessed: " + isGuessed)
  }

  def snapshot(): Cell = {
    val c = new Cell(this.row, this.col)
    c.guess = this.guess
    c.guesses = Set(this.guesses.toList:_*)
    c
  }

  def markComplete() = num = guess
  override def hashCode(): Int = (row * 7) + (col * 13)
  override def equals(o: Any) = {
    if (o.isInstanceOf[Cell]) {
      o.asInstanceOf[Cell].hashCode == this.hashCode
    } else {
      false
    }
  }
  def toEntry(): Tuple2[Int, Cell] = (hashCode(), this)
}