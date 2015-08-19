package scala.sudoku

import scala.collection.immutable._

sealed abstract class Holder {
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
    toNums(cols(c.col)) ++ toNums(rows(c.row)) ++ toNums(squares(c.square))
  }

  def toNums(s: TreeSet[Cell]) = getNums(s).map(_ match {
    case -1 => None
    case x => Some(x)
  }).flatten

  def getNums(s: TreeSet[Cell]): Set[Int]

  def addCell(c: Cell) = {
    cols(c.col) = cols(c.col) + c
    rows(c.row) = rows(c.row) + c
    squares(c.square) = squares(c.square) + c
  }

  def removeCell(c: Cell) = {
    cols(c.col) = cols(c.col) - c
    rows(c.row) = rows(c.row) - c
    squares(c.square) = squares(c.square) - c
  }

  // private def getSquare(c: Cell) = {
  //   getRowSquare(c.row).intersect(getColSquare(c.col)).head
  // }

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