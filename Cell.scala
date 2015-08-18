package scala.sudoku
import scala.math.Ordered.orderingToOrdered

class Cell(val row: Int,val  col: Int) extends Ordered[Cell] {
  private var possible: Set[Int] = Set.empty[Int]
  private var num: Int = -1

  def this(initNum: Int, row: Int, col: Int) {
    this(row, col);
    num = initNum
  }
  

  def setPossible(pos: Set[Int]) = {
    possible = pos

    possible.size match {
      case 0 => throw new RuntimeException("Not possible to have no possible")
      case 1 => num = possible.head
      case _ => ;
    }
  }

  def isFound() = num > 0

  def getNum() = num

  def compare(that: Cell): Int = (this.row, this.col) compare (that.row, that.col)
  // private var testing: Option[Int] = None


  // def isFound = number match {
  //   case Some(_) => true
  //   case _ => possible.length == 1
  // }

  // def setTesting(t: Int) = {
  //   testing = t
  // }

  // def start() = preSet = possible.filter(_ => true)

  // def removeOverlap()
}