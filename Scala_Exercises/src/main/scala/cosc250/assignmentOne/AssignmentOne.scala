package cosc250.assignmentOne

import scala.annotation.tailrec

object AssignmentOne {

  /**
    *  This is a sequence that John Conway (famous for the game of life) offered a prize about at a symposium at Bell
    *  Labs a long time ago.
    *
    *  a(1) = 1
    *  a(2) = 1
    *  a(n) = a(a(n - 1)) + a(n - a(n - 1)), for n > 2
    *
    *  You can find more on the sequence here
    *  https://oeis.org/A004001
    *
    *  Note that this problem is "1-indexed" -- ie, we're only defining the sequence starting at 1 instead of 0
    *
    *  Your function should produce the sequence from 1 to n (inclusive)
    *
    *  For style marks, use some memoisation so that you don't keep recomputing the same values.
    *  (But you don't need to produce a tail recursive solution)
    */
/*
conwaySequence:
This takes an integer to be the position in the Conway Sequence.
The Conway Sequence function is then applied to return the full
sequence up to the position inputted.
For example:
conwaySequence(10) should be (Seq(1, 1, 2, 2, 3, 4, 4, 4, 5, 6))
 */
def conwaySequence(n:Int):Seq[Int] = {
  @tailrec
  // Collects the numbers in a sequence to avoid recalculation.
  def memoised(target:Int, i:Int, seq:Seq[Int]):Seq[Int] = {
    if (target == i) {
      seq
    }
    else {
      val nextVal =

      // this is the conway sequence formula
        if (i > 2) {
          seq(seq(i - 1)) + seq(i - seq(i - 1))
        }
        else 1

      // when the target has not been reached,
      // keep generating the sequence
      memoised(target, i + 1, seq :+ nextVal)
    }
  }
  // The head element is removed because this
  // problem is "1-indexed"
  memoised(n + 1, 0, Seq.empty).tail
}
  /**
    * Use foldLeft to sum the number of characters in a tree of strings
    */
  /*
  countOfTree:
  This applies foldLeft to concatenate all strings into one.
  The resulting string's length is then returned.
  For example:
  countOfTree(TreeSet("SUPERCALIFRAGILISTICEXPIALIDOTIOUS")) should be (34)
   */
  def countOfTree(tree:scala.collection.immutable.TreeSet[String]):Int = {
    tree.foldLeft("")(_+_).length
  }

  /**
    * Let's implement a Scrabble scorer.
    * It should take into account the letters in the word *and* the squares the word sits on.
    *
    * I've created a sealed trait to model the different kinds of square. A "sealed trait" means that every class or
    * object that implements that trait is defined in the same program file. Knowing that there aren't any other
    * potential Squares out there (eg, being added later by other programmers) means the compiler can do cleverer
    * exhaustiveness-checking for us.
    *
    */

  sealed trait Square
  case object OrdinarySquare extends Square
  case object DoubleLetterScore extends Square
  case object TripleLetterScore extends Square
  case object DoubleWordScore extends Square
  case object TripleWordScore extends Square

  /**
   * (You may assume all letters are uppercase)
   *
   * 1: A, E, I, O, U, L, N, S, T, R.
   * 2: D, G.
   * 3: B, C, M, P.
   * 4: F, H, V, W, Y.
   * 5: K
   * 8: J, X.
   * 10: Q, Z.
   *
   * You might find using "mystring".contains(char) useful to keep it short
   */
  /*
  letterScore:
  This returns the score for the inputted character
  according to task instruction.
  For example:
  letterScore('Q') should be (10)

   */
  def letterScore(char:Char):Int = char match {
    case x if "AEIOULNSTR".contains(char) => 1
    case x if "DG".contains(char) => 2
    case x if "BCMP".contains(char) => 3
    case x if "FHVWY".contains(char) => 4
    case x if "K".contains(char) => 5
    case x if "JX".contains(char) => 8
    case x if "QZ".contains(char) => 10
      // returns score based on letter
  }

  /**
    * This should work out what this letter scores, given the square it's on.
    * Don't forget - DoubleWordScores etc affect the word as a whole, not individual letters
    */
  /*
  letterAndSquareScore:
  This applies a score multiplier depending on the inputted square.
  Word bonuses should be ignored for now (applied in a later function).
  For example:
  letterAndSquareScore('X', TripleLetterScore) should be (24)
   */
  def letterAndSquareScore(char:Char, sq:Square):Int = sq match {
    case DoubleLetterScore => letterScore(char) * 2
    case TripleLetterScore => letterScore(char) * 3
    case _ => letterScore(char)
  }

  /**
    * Calculate the scrabble score for a word on a set of squares.
    *
    * Hint: the zip method on Seq will zip to sequences together into a sequence of tuples. For example,
    *
    * Seq(1, 2, 3).zip(Seq("A", "B", "C")) produces Seq((1, "A"), (2, "B"), (3, "C")).
    *
    * If the sequences are of lengths, it'll just zip as much as it can. For example,
    * Seq(1, 2).zip(Seq("A", "B", "C")) produces Seq((1, "A"), (2, "B")).
    *
    * Tuples can be accessed using _1 and _2
    * val tup = (1, 2)
    * tup._1 == 1
    *
    * or using destructuring assignmnet
    * val (x, y) = tup
    *
    */
  /*
  scrabbleScore:
  This calculates the score for a given string depending on
  a sequence of squares.
  letterAndSquareScore is applied to each character.
  Word bonus multipliers are applied on the total score.
  For example:
  scrabbleScore("HERRING", Seq(DoubleWordScore,
  TripleWordScore, DoubleLetterScore, OrdinarySquare,
  OrdinarySquare, OrdinarySquare, TripleLetterScore)) should be (96)
   */
  def scrabbleScore(word:String, squares:Seq[Square]):Int = {
    val doubleScore = 2 * squares.count(_ == DoubleWordScore)
    val tripleScore = 3 * squares.count(_ == TripleWordScore)
    val letterScore =
      word.zip(squares).map({case (w,s) => letterAndSquareScore(w,s)}).sum
    Seq(doubleScore, tripleScore, letterScore).filter(_ > 0).product
  }

  /**
   * Let's solve the "8 queens" problem -- how to put eight queens on a chessboard without any of them attacking
   * each other.
   */

  /**
    * A position on the board. For our purposes, columns and rows are numbered from 1 to 8
    */
  case class Pos(x:Int, y:Int)

  /** Are two positions in the same row? */
  /*
  sameRow:
  Compares the rows of two positions and returns a Boolean.
  For example:
  sameRow(Pos(1, 1), Pos(1, 7)) should be (true)
   */
  def sameRow(p1:Pos, p2:Pos):Boolean = {
    p1.x == p2.x
  }

  /** Are two positions in the same column? */
  /*
  sameCol:
  Compares the columns of two positions and returns a Boolean.
  For example:
  sameRow(Pos(1, 1), Pos(7, 1)) should be (true)
  */
  def sameCol(p1:Pos, p2:Pos):Boolean = {
    p1.y == p2.y
  }

  /** Are two positions on the same diagonal? Remember, there are two diagonals to worry about. */
  /*
  sameDiagonal:
  Compares the diagonals of two positions and returns a Boolean.
  For example:
  sameRow(Pos(1, 7), Pos(7, 1)) should be (true)
  */
  def sameDiagonal(p1:Pos, p2:Pos):Boolean = {
    (p1.x - p2.x).abs == (p1.y - p2.y).abs
  }

  /**
   * Now let's define a function to test whether queens in two positions are attacking each other.
   * Don't forget a queen cannot attack itself. ie, (4,4) is not attacking (4,4)
   */
  /*
  sameDiagonal:
  Compares the rows, columns and diagonals of
  two positions and returns a Boolean.
  Disregards same positions.
  For example:
  attackingEachOther(Pos(1, 1), Pos(7, 7)) should be (true)
  */
  def attackingEachOther(p1:Pos, p2:Pos):Boolean = {
    if (p1 == p2) false
    else sameRow(p1,p2) || sameCol(p1,p2) || sameDiagonal(p1,p2)
  }

  /**
    * Using your attackingEachOther method, write a function that looks through a sequence of positions and finds if
    * there are any queens attacking each other
    */
  /*
  seqContainsAttack:
  This analyzes a sequence of positions by iterating through
  each possible combination to check for an attack.
  At least one attack returns true.
  For example:
  seqContainsAttack(Seq(Pos(1, 1), Pos(7, 7), Pos(2, 5))) should be (true)
   */
  def seqContainsAttack(queens:Seq[Pos]):Boolean = {
    (for {
      x <- queens
      y <- queens
    } yield attackingEachOther(x,y)).foldLeft(false)(_||_)
  }

  /**
    * This method should take a sequence of rows. To solve eight queens, all of the queens must be in different columns.
    * So, rather input the full positions for each queen, we can just take (in order) the row number for each column.
    * ie, Seq(1, 8, 2, 7) would mean there's a queen at (1,1) another at (2, 7), another at (3, 2), another at (4, 7).
    *
    * Use your seqContainsAttack function to work out whether a sequence in this format contains an attack. You might
    * find the "zipWithIndex" function helpful. This starts at zero, not one, which might or might not make a difference.
    */
  /*
  seqContainsAttackI:
  This interprets a sequence of integers as a sequence of
  positions to apply seqContainsAttack.
  For example:
  seqContainsAttackI(Seq(1, 3, 2, 4)) should be (true)
   */
  def seqContainsAttackI(queens:Seq[Int]):Boolean = {
    seqContainsAttack(queens.zipWithIndex.map({case (x,y)=> Pos(x,y)}))
  }
  /**
    * Now we're going to use another trick to make the whole computation very small. As well as the queens all being
    * in different columns, they're also all in different rows. So every solution is going to be a permutation of
    * Seq(1, 2, 3, 4, 5, 6, 7, 8). But we're going to need to filter the permutations to only the ones that work.
    *
    * Write your function to calculate all 92 solutions to the eight queens problem
    *
    * You might find the following useful:
    * - permutations will produce an Iterator across the permutations of a sequence
    * - filterNot will filter a Seq, Iterator, etc, to only those where a particular function returns false
    * - toSeq will turn a List, Iterator, etc, into a Seq (sequence)
    */
  /*
  eightQueens:
  This filters out those sequence permutations that contain an attack,
  and creates a sequence of those sequence permutations that win the game.
  For example:
  eightQueens.count(_ => true) should be (92)
  and
  eightQueens.contains(Seq(7, 1, 3, 8, 6, 4, 2, 5)) should be (true)
   */
  def eightQueens:Seq[Seq[Int]] = {
    Seq(1,2,3,4,5,6,7,8).permutations.filterNot(p => seqContainsAttackI(p)).toSeq
  }


  /**
   * Given two strings of equal length, calculate the number of characters that are different in each string.
   *
   * eg, "BLAB" and "BLOB" are 1 character different. "RAM" and "MAR" are 2 characters different.
   */
  /*
  numDiffChars:
  This zips each string character-to-character in order.
  These are then compared and counted if different.
  For example:
  numDiffChars("BULL", "BAIT") should be (3)
   */
  def numDiffChars(a:String, b:String):Int = {
    a.zip(b).count({ case (x, y) =>  x != y })
  }

  /**
    * I'll give you this method -- it takes a sequence and returns a sequence of tuples, with each pair. eg,
    * seqToPairs(Seq(1, 2, 3)) would produce Seq((1, 2), (2, 3))
    */
  def seqToPairs[T](s:Seq[T]):Seq[(T,T)] = s zip s.tail

  /**
   * Given a sequence of strings, sum the character changes that would be needed to change each string to the next
   * eg:
   *
   * SUM
   * SAM  (1 change)
   * BAT  (2 changes)
   * total: 3 changes
   */
  /*
  sumChanges:
  This applies numDiffChars to pairs in a sequence,
  then sums the total changes.
  For example:
  sumChanges(Seq("BALL", "BAIT", "BOLT")) should be (4)
   */
  def sumChanges(s:Seq[String]):Int = {
    seqToPairs(s).map({case(x,y)=>numDiffChars(x,y)}).sum
  }
  /**
    * Given a sequence of strings, reorder the strings so that sumChanges will be as small as possible.
    * A sequence and its reverse will have the same length.
    */
  /*
  smallestChanges:
  This takes a string of sequences and indexes each permutation.
  The index of the permutation with smallest sumChanges value is
  used to return its associated string.
  For example:
  sumChanges(smallestChanges(Seq("BALL", "WASH", "BALK", "WALK")))
  should be (sumChanges(Seq("BALL", "BALK", "WALK", "WASH")))
   */
  def smallestChanges(strings:Seq[String]):Seq[String] = {
    val seqOfPerms = strings.permutations.toSeq.zipWithIndex
    val smallestSeq = seqOfPerms.map({case(x,y)=> (sumChanges(x),y)}).min._2
    seqOfPerms(smallestSeq)._1
  }


  /**
   * Now lets make a little calculator
   * Again I've got you started by defining a sealed trait and a few case classes
   */
  sealed trait Expression {

    /** result should calculate the answer. You should implement it in the subclasses. */
    def result:Double

    /**
      * Implement foldLeft... If you've got it right, then count will work
      *
      * Normally, we define foldLeft on a list, but actually it can be generalised to data types such as trees.
      * Technically, it then gets called a "catamorphism", but let's just stick with foldLeft
      *
      * You might find it helpful to sketch a little expression tree, and then think about
      * - what you would do for leaf nodes
      *   hint: you can work that out from the types --- you have a function that takes a value, and a node containing a value
      * - what you would do for non-leaf nodes
      *   hint: just work it out for the three-node tree. And remember you are processing from left to right
      *
      *
      */
    def foldLeft[A](start:A)(f:(A, Expression) => A):A

    /** Traverses the nodes from left to right, counting them if they match a condition. */
    def count(f:Expression => Boolean) = foldLeft(0) {
      case (counted, expr) if f(expr) => counted + 1
      case (counted, _) => counted
    }
  }

  case class Number(i:Int) extends Expression {

    // returns the int input
    override def result = i

    // feeds Number(0) as a "null value" expression to identify it as a Number Expression
    override def foldLeft[A](start:A)(f:(A, Expression) => A) = {
      f(start, Number(0))
    }
  }
  case class Add(left:Expression, right:Expression) extends Expression {

    // returns the sum of the left and right expressions
    override def result = left.result + right.result

    // implements an Add Expression version of the built-in Scala function foldLeft
    override def foldLeft[A](start:A)(f:(A, Expression) => A) = {

      // initialize a mutable accumulator
      var accum = start

      // leftSeq and rightSeq:
      // create a sequence of expressions based on the type of Expression, e.g. Number or Add
      // each element in the sequence is a "null value" expression used only for type identification
      val leftSeq = left.foldLeft(Seq.empty[Expression])({
        case (seq, expr) => seq :+ expr })
      val rightSeq = right.foldLeft(Seq.empty[Expression])({
        case (seq, expr) => seq :+ expr })

      // take this object's expression type along with expression sequences created from this
      // object's left and right expressions and then combine into a mutable "expression tree" sequence
      var exprTree = Seq[Expression](Add(Number(0),Number(0))) ++ leftSeq ++ rightSeq

      // apply the foldLeft's parameter function to each expression in the expression tree
      while (!exprTree.isEmpty) {
        accum = f(accum, exprTree.head)
        exprTree = exprTree.tail
      }
      // return the accumulator
      accum
    }
  }
  case class Multiply(left:Expression, right:Expression) extends Expression {

    // returns the product of the left and right expressions
    override def result = left.result * right.result

    // implements a Multiply Expression version of the built-in Scala function foldLeft
    override def foldLeft[A](start:A)(f:(A, Expression) => A) = {

      // initialize a mutable accumulator
      var accum = start

      // leftSeq and rightSeq:
      // create a sequence of expressions based on the type of Expression, e.g. Number or Add
      // each element in the sequence is a "null value" expression used only for type identification
      val leftSeq = left.foldLeft(Seq.empty[Expression])({
        case (seq, expr) => seq :+ expr })
      val rightSeq = right.foldLeft(Seq.empty[Expression])({
        case (seq, expr) => seq :+ expr })

      // take this object's expression type along with expression sequences created from this
      // object's left and right expressions and then combine into a mutable "expression tree" sequence
      var exprTree = Seq[Expression](Multiply(Number(0),Number(0))) ++ leftSeq ++ rightSeq

      // apply the foldLeft's parameter function to each expression in the expression tree
      while (!exprTree.isEmpty) {
        accum = f(accum, exprTree.head)
        exprTree = exprTree.tail
      }
      // return the accumulator
      accum
    }

  }
  case class Subtract(left:Expression, right:Expression) extends Expression {
    // returns the product of the left and right expressions
    override def result = left.result - right.result

    // implements a Multiply Expression version of the built-in Scala function foldLeft
    override def foldLeft[A](start:A)(f:(A, Expression) => A) = {

      // initialize a mutable accumulator
      var accum = start

      // leftSeq and rightSeq:
      // create a sequence of expressions based on the type of Expression, e.g. Number or Add
      // each element in the sequence is a "null value" expression used only for type identification
      val leftSeq = left.foldLeft(Seq.empty[Expression])({
        case (seq, expr) => seq :+ expr })
      val rightSeq = right.foldLeft(Seq.empty[Expression])({
        case (seq, expr) => seq :+ expr })

      // take this object's expression type along with expression sequences created from this
      // object's left and right expressions and then combine into a mutable "expression tree" sequence
      var exprTree = Seq[Expression](Subtract(Number(0),Number(0))) ++ leftSeq ++ rightSeq

      // apply the foldLeft's parameter function to each expression in the expression tree
      while (!exprTree.isEmpty) {
        accum = f(accum, exprTree.head)
        exprTree = exprTree.tail
      }
      // return the accumulator
      accum
    }
  }

  /** Calculate the result of an expression */
  /*
  calculate:
  This returns the result field for the calling Expression object.
  For example:
  calculate(Add(Number(3), Multiply(Number(7), Number(2)))) should be (17)
   */
  def calculate(ex:Expression) = ex.result

  /** Now implement a function that will count how many Multiply nodes there are in the calculation */
  /*
  countMultiplications
  This takes an Expression object and calls its foldLeft function
  to return the number of Multiply Expression that the object contains.
  For example:
  countMultiplications(Multiply(Number(3), Multiply(Number(7), Number(2))))
  should be (2)
   */
  def countMultiplications(ex:Expression):Int = {

    //
    ex.count(_ == Multiply(Number(0),Number(0)))
  }


  /**
    * We started with John Conway -- let's finish with his most famous creation: Conway's Game of Life.
    * https://en.wikipedia.org/wiki/Conway's_Game_of_Life
    *
    * Suppose we have a grid of squares, say 20 by 20
    * And a square can be filled in (alive) or not filled in (dead).
    *
    * And at each "time step", we generate a new grid using the following rules:
    * Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
    * Any live cell with two or three live neighbours lives on to the next generation.
    * Any live cell with more than three live neighbours dies, as if by overpopulation.
    * Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
    * (Each cell has eight neighbours)
    *
    * We're going to define the game using an immutable Map.
    * Here, I've used Scala's "type alias" syntax to say that a ConwayState is a map from a tuple of ints to a boolean
    *
    * The tuple is going to contain (x, y) coordinates, and the Boolean is going to contain the values.
    * If an element in the map is missing, assume it to be false (dead). You can use getOrElse for this. This also
    * has the advantage that we *can* ask about negative indices -- getOrElse((-1, -1), false) will be false
    *
    */
  type ConwayState = Map[(Int, Int), Boolean]

  /**
    * Blinkers have a habit of toggling -- to help you test your code, I've included their definition.
    * If you have a blinker1, and you move forward one tick in the game state, you should get blinker2.
    * See the wikipedia page for more on this.
    */
  val blinker1:ConwayState = Map(
    (2, 1) -> true, (2, 2) -> true, (2, 3) -> true
  )
  val blinker2:ConwayState = Map(
    (1, 2) -> true, (2, 2) -> true, (3, 2) -> true
  )

  /**
    * First, define a function that given a tuple and a ConwayState will count the number of live neighbours
    */
  /*
  liveNeighbours:
  This first creates a sequence of a states key values (excluding itself).
  Then, it counts those positions in the state that meet the 'criteria'
  for being a neighbour as specified in the task instructions.
  For example:
  liveNeighbours((2, 2), blinker1) should be (2)
   */
  def liveNeighbours(pos:(Int, Int), state:ConwayState):Int = {
    state.keys.toSeq.filterNot(p => p == pos).count({
      case p => ((p._1 - pos._1).abs < 2) && ((p._2 - pos._2).abs < 2)
    })
  }

  /**
    * Next, define a function that determines whether a position should be alive or dead
    */
  /*
  aliveOrDead:
  This matches liveNeighbour results to criteria given in the task instructions
  to determine alive or dead status.
  For example:
  aliveOrDead((2, 2), blinker1) should be (true)
   */
  def aliveOrDead(pos:(Int, Int), state:ConwayState):Boolean = liveNeighbours(pos, state) match {
    case x if x < 2 => false
    case 2 =>
      if (state.contains(pos)) true
      else false
    case 3 => true
    case x if x > 4 => false
  }

  /**
    * Next, define a function that will compute the next state of the game of life, for a given maximum X and Y
    */
  /*
  nextConwayState:
  This analyzes every position, determining if alive or dead.
  The results are outputted as a map.
  For example:
  nextConwayState(blinker1) should be (blinker2)
   */
  def nextConwayState(state:ConwayState, maxSize:(Int, Int) = (20, 20)):ConwayState = {
    (for {
      x <- 0 until maxSize._1
      y <- 0 until maxSize._2
    } yield (x,y) -> aliveOrDead((x,y), state)).toMap
  }
}
