package cosc250.assignmentOne

import org.scalatest._

import scala.collection.immutable.TreeSet


/**
  * This is a specification file for ScalaTest. It's a set of unit tests written in a way that's designed to be
  * read easily.
  */
class AssignmentOneSpec extends FlatSpec with Matchers {

  import AssignmentOne._

  "conwaySequence" should "return the first 10 elements in the sequence when asked"  in {
    conwaySequence(10) should be (Seq(1, 1, 2, 2, 3, 4, 4, 4, 5, 6))
  }


  "countOfTree" should "count AND DID THOSE FEET as 15" in {
    countOfTree(TreeSet("AND", "DID", "THOSE", "FEET")) should be (15)
  }
  it should "count the empty Tree as 0" in {
    countOfTree(TreeSet()) should be (0)
  }
  it should "count SUPERCALIFRAGILISTICEXPIALIDOTIOUS as 34" in {
    countOfTree(TreeSet("SUPERCALIFRAGILISTICEXPIALIDOTIOUS")) should be (34)
  }

  "letterScore" should "score X as 8" in { letterScore('X') should be(8) }
  it should "score Q as 10" in { letterScore('Q') should be(10) }
  it should "score R as 1" in { letterScore('R') should be(1) }
  it should "score S as 1" in { letterScore('S') should be(1) }

  "letterAndSquareScore" should "score X on a double letter score as 16" in {
    letterAndSquareScore('X', DoubleLetterScore) should be (16)
  }
  it should "score X on a double word score as 8" in {
    letterAndSquareScore('X', DoubleWordScore) should be (8)
  }
  it should "score X on a triple letter score as 8" in {
    letterAndSquareScore('X', TripleLetterScore) should be (24)
  }


  "Scrabble" should "score XYLOPHONE with no special squares as 24" in {
    scrabbleScore("XYLOPHONE", "XYLOPHONE".map(_ => OrdinarySquare)) should be (24)
  }
  it should "score HERRING with no special squares as 11" in {
    scrabbleScore("HERRING", "HERRING".map(_ => OrdinarySquare)) should be (11)
  }
  it should "score HERRING with 1 double letter on R and 1 double letter on G as 14" in {
    scrabbleScore("HERRING", Seq(OrdinarySquare, OrdinarySquare, DoubleLetterScore, OrdinarySquare, OrdinarySquare, OrdinarySquare, DoubleLetterScore)) should be (14)
  }
  it should "score HERRING with 1 double letter on R and 1 triple letter on G as 16" in {
    scrabbleScore("HERRING", Seq(OrdinarySquare, OrdinarySquare, DoubleLetterScore, OrdinarySquare, OrdinarySquare, OrdinarySquare, TripleLetterScore)) should be (16)
  }
  it should "score HERRING with 1 double letter on R and 1 triple letter on G, and a double word score" in {
    scrabbleScore("HERRING", Seq(DoubleWordScore, OrdinarySquare, DoubleLetterScore, OrdinarySquare, OrdinarySquare, OrdinarySquare, TripleLetterScore)) should be (32)
  }
  it should "score HERRING with 1 double letter on R and 1 triple letter on G, and 1 double word score and 1 triple word score" in {
    scrabbleScore("HERRING", Seq(DoubleWordScore, TripleWordScore, DoubleLetterScore, OrdinarySquare, OrdinarySquare, OrdinarySquare, TripleLetterScore)) should be (96)
  }

  "attackingEachOther" should "think 1,1 and 1,7 are attacking each other" in {
    attackingEachOther(Pos(1, 1), Pos(1, 7)) should be (true)
  }
  it should "think 7,1 and 7,1 are not attacking each other" in {
    attackingEachOther(Pos(7, 1), Pos(7, 1)) should be (false)
  }
  it should "think 7,1 and 5,2 are not attacking each other" in {
    attackingEachOther(Pos(7, 1), Pos(5, 2)) should be (false)
  }
  it should "think 7,1 and 2,2 are not attacking each other" in {
    attackingEachOther(Pos(7, 1), Pos(2, 2)) should be (false)
  }
  it should "think 1,1 and 7,1 are attacking each other" in {
    attackingEachOther(Pos(1, 1), Pos(7, 1)) should be (true)
  }
  it should "think 1,1 and 7,7 are attacking each other" in {
    attackingEachOther(Pos(1, 1), Pos(7, 7)) should be (true)
  }
  it should "think 1,7 and 7,1 are attacking each other" in {
    attackingEachOther(Pos(1, 7), Pos(7, 1)) should be (true)
  }

  "seqContainsAttackI" should "think that Seq(1, 1) contains an attack" in {
    seqContainsAttackI(Seq(1, 1)) should be (true)
  }
  it should "think that Seq(1, 3, 2, 4) contains an attack" in {
    seqContainsAttackI(Seq(1, 3, 2, 4)) should be (true)
  }
  it should "think that Seq(1, 3, 2) contains an attack" in {
    seqContainsAttackI(Seq(1, 3, 2)) should be (true)
  }
  it should "think that Seq(1) does not contain an attack" in {
    seqContainsAttackI(Seq(1)) should be (false)
  }
  it should "think that Seq(1, 3, 5) does not contain an attack" in {
    seqContainsAttackI(Seq(1, 3, 5)) should be (false)
  }

  "eightQueens" should "find 92 solutions" in {
    eightQueens.count(_ => true) should be (92)
  }
  it should "contain 2, 4, 6, 8, 3, 1, 7, 5" in {
    eightQueens.contains(Seq(2, 4, 6, 8, 3, 1, 7, 5)) should be (true)
  }
  it should "contain 1, 7, 4, 6, 8, 2, 5, 3" in {
    eightQueens.contains(Seq(1, 7, 4, 6, 8, 2, 5, 3)) should be (true)
  }
  it should "contain 7, 1, 3, 8, 6, 4, 2, 5" in {
    eightQueens.contains(Seq(7, 1, 3, 8, 6, 4, 2, 5)) should be (true)
  }


  "numDiffChars" should "find no differences between BALL and BALL" in {
    numDiffChars("BALL", "BALL") should be (0)
  }
  it should "find one difference between BULL and BALL" in {
    numDiffChars("BULL", "BALL") should be (1)
  }
  it should "find 3 differences between BULL and BAIT" in {
    numDiffChars("BULL", "BAIT") should be (3)
  }

  "sumChanges" should "find no differences in (BALL, BALL, BALL)" in {
    sumChanges(Seq("BALL", "BALL", "BALL")) should be (0)
  }
  it should "find no differences in (BALL)" in {
    sumChanges(Seq("BALL")) should be (0)
  }
  it should "find 4 differences in (BALL, BAIT, BOLT)" in {
    sumChanges(Seq("BALL", "BAIT", "BOLT")) should be (4)
  }

  "smallestChanges" should "transform (BALL, WASH, BALK, WALK) into (BALL, BALK, WALK, WASH)" in {
    sumChanges(smallestChanges(Seq("BALL", "WASH", "BALK", "WALK"))) should be (sumChanges(Seq("BALL", "BALK", "WALK", "WASH")))
  }


  "calculate" should "find 3 + 7 * 2 = 17" in {
    calculate(Add(Number(3), Multiply(Number(7), Number(2)))) should be (17)
  }

  "countMultiply" should "find 3 + 7 * 2 has 1 multiply" in {
    countMultiplications(Add(Number(3), Multiply(Number(7), Number(2)))) should be (1)
  }
  it should "find 3 * 7 * 2 has 2 multiply" in {
    countMultiplications(Multiply(Number(3), Multiply(Number(7), Number(2)))) should be (2)
  }
  it should "find 3 + 7 + 2 has 0 multiply" in {
    countMultiplications(Add(Number(3), Add(Number(7), Number(2)))) should be (0)
  }

  "Game of Life" should "find that the top of a blinker1 has one neighbour" in {
    liveNeighbours((2, 1), blinker1) should be (1)
  }

  it should "find that the middle of a blinker1 has two neighbours" in {
    liveNeighbours((2, 2), blinker1) should be (2)
  }

  it should "find that the top of a blinker1 will die" in {
    aliveOrDead((2, 1), blinker1) should be (false)
  }

  it should "find that the middle of a blinker1 will stay alive" in {
    aliveOrDead((2, 2), blinker1) should be (true)
  }

  it should "find that blinkers oscillate" in {

    def filledIn(m:ConwayState, size:(Int, Int) = (20, 20)):ConwayState = {
      (for { x <- 0 until size._1; y <- 0 until size._2 } yield (x, y) -> m.getOrElse((x, y), false)).toMap
    }

    nextConwayState(blinker1) should be (filledIn(blinker2))
    nextConwayState(blinker2) should be (filledIn(blinker1))
  }

}
