import java.io.{BufferedReader, InputStreamReader}

class GameBoard(val maxSize: Int, val input: String, val its: Int) {
  var boardSize: Int = maxSize * maxSize
  var matrix: Array[Boolean] = new Array[Boolean](boardSize)
  var lives: Array[Int] = input.split(",").map(x => x.toInt)

  for (cell <- lives) {
    matrix(cell) = true
  }
  printBoard(this.matrix)

  /**
   * This method starts the game loop until maximum iteration is reached.
   */
  def gameLoop(): Unit = {
    var currIts = 0
    while (currIts <= its) {
      updateBoard()
      printBoard(this.matrix)
      currIts += 1
    }
  }

  /**
   * This method updates the board by changing matrix with board state.
   */
  def updateBoard(): Unit = {
    var survives: List[Int] = List()
    for (i <- this.matrix.indices) {
      // checks neighbors
      var neighbors = 0
      if (this.matrix((boardSize + i - 1) % boardSize)) { //left
        neighbors += 1
      }
      if (this.matrix((boardSize + i - this.maxSize - 1) % boardSize)) { //left-above
        neighbors += 1
      }
      if (this.matrix((boardSize + i + this.maxSize - 1) % boardSize)) { //left-below
        neighbors += 1
      }
      if (this.matrix((i + 1) % boardSize)) { //right
        neighbors += 1
      }
      if (i > this.maxSize && this.matrix((boardSize + i - this.maxSize + 1) % boardSize)) { //right-above
        neighbors += 1
      }
      if (this.matrix((i + this.maxSize + 1) % boardSize)) { //right-below
        neighbors += 1
      }
      if (this.matrix((boardSize + i - this.maxSize) % boardSize)){ //above
        neighbors += 1
      }
      if (this.matrix((i + this.maxSize) % boardSize)) { //below
        neighbors += 1
      }
      // survives + reproduce based on neighbors
      if (this.matrix(i) && neighbors == 2) {
        survives = i :: survives
      } else if (neighbors == 3) {
        survives = i :: survives
      }
    }
    val nextGen = newGrid()
    for (index <- survives) {
      nextGen(index) = true
    }
    this.matrix = nextGen
  }

  /**
   * This method creates a new array with length boardSize, with each element being false.
   * @return an array of all false
   */
  def newGrid(): Array[Boolean] = {
    val grid = new Array[Boolean](boardSize)
    for (i <- 0 until boardSize) {
      grid(i) = false
    }
    grid
  }

  /**
   * This method prints the board.
   * @param grid an array that represents a board state.
   */
  def printBoard(grid: Array[Boolean]): Unit = {
    var gridVis: String = " "
    for (i <- 0 until maxSize) {
      gridVis += "__"
    }
    gridVis += "\n|"
    // goes through each cell, 0 for true, space for false
    var i = 1
    while (i <= this.boardSize) {
      if (grid(i - 1)) {
        gridVis += "O "
      } else {
        gridVis += "  "
      }
      if (i % this.maxSize == 0 && i != this.boardSize) {
        gridVis += "|\n|"
      }
      if (i == this.boardSize){
        gridVis += "|\n "
        for (i <- 0 until maxSize) {
          gridVis += "--"
        }
        gridVis += "\n"
      }
      i += 1
    }
    print(gridVis)
  }
}

object Main extends App {
  println("This is Life on a Donut by Beanway144.")
  val bReader = new BufferedReader(new InputStreamReader(System.in))

  // gets grid size
  print("Enter grid size: ")
  val maxSize = bReader.readLine().toInt

  // prints out initial grid with enumeration
  var i = 1
  var gridVis: String = ""
  while (i <= maxSize * maxSize) {
    gridVis += i.toString + " "
    if (i < 10) {
      gridVis += " "
    }
    if (i < 100) {
      gridVis += " "
    }
    if (i % maxSize == 0){
      gridVis += "\n"
    }
    i += 1
  }
  println(gridVis)

  // gets alive cells
  print("Enter coordinates of alive cells, separated by commas: ")
  val aliveCells = bReader.readLine()

  // get iterations
  print("Enter number of iterations: ")
  val its = bReader.readLine().toInt

  bReader.close()

  val gb = new GameBoard(maxSize, aliveCells, its)
  gb.gameLoop()

}