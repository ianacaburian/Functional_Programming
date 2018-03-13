import scala.annotation.tailrec
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import scala.util.Random
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object MyApp extends App {

  Console.println("As this extends App, the object's body is run at startup.")

  // This creates an Actor System
  val system = ActorSystem.apply("SpitSystem")

  // The rest is over to you...
  implicit val timeout = Timeout(1.seconds)
  /**Types
    * Option was chosen for Card type as there needs to be the possibility of "no" card
    * that would not return errors, or require exception handling.
    * */
  type Card = Option[(Int, Char)]
  type Pile = List[Card]
  type Layout = List[Pile]

  /**
    * Utility Methods
    * toString methods for presenting cards in a readable manner.
    * ValidCardNums is a global utitlity method used by both actors to determine which numbers would
    * make for valid card choices when discarding onto a pile.
    * Although a "proper" solution would be to use some form of a circular list, there's only 2
    * edge cases: the number 1 and 13 to handle, which is simple enough to be handled "manually"
    * */
  def cardToString(card:Card):String = card match{
    case None => "_ "
    case Some((13,suit)) => s"K$suit"
    case Some((12,suit)) => s"Q$suit"
    case Some((11,suit)) => s"J$suit"
    case Some((10,suit)) => s"T$suit"
    case Some((1,suit)) => s"A$suit"
    case Some((num,suit)) => s"$num$suit"
  }
  def layoutToString(layout:Layout):String = {
    layout.foldLeft(""){
      (string, pile) => pile.length match {
        case 0 => string + "_ "
        case 1 => string + cardToString(pile.head) + " "
        case _ => string + cardToString(pile.head) + "."*pile.tail.length + " "
      }
    }
  }
  def dcPilesToString(pileCards:(Card, Card), pileLengths:(Int, Int)):String = {
    s"[1: ${cardToString(pileCards._1)} +${pileLengths._1}] " +
      s"[2: ${cardToString(pileCards._2)} +${pileLengths._2}]"
  }
  def validCardNums(card:Card):List[Int] = card match {
    case None => List(0)
    case c if c.get._1 == 1 => List(13, 2)
    case c if c.get._1 == 13 => List(12, 1)
    case c => List(c.get._1-1, c.get._1+1)
  }

  /**
    * Actor messages.
    * Many parameters serve for logging purposes only -- particularly PlayCards.
    * This is to minimize the scattering of println statements throughout the code in order to
    * group all logging information in roughly the same place.
    * Purposes for each are explained via println statements within (while also serving for debugging)*/

  case class StartRound(player1Pile:Pile, player2Pile:Pile)
  case class DealCards(newDeck:Pile, newLayout:Layout)
  case class ReadyToPlay(player:ActorRef)
  case class DeckTail(deckHead:Card, deckLength:Int, dcPileLengths:(Int,Int))
  case class PlayCards(messageID:String,
                       moveCount:Int, compareLayouts:(Layout,Layout),
                       dcPileCards:(Card,Card), dcPileLengths:(Int,Int),
                       cardAndInt:(Card, Int))
  case class DiscardCard(dcPileCards:(Card,Card), dcPileLengths:(Int,Int))
  case class CheckDiscard(cardChoice:Card, pileChoice:Int, currentLayout:Layout)
  case class DiscardApproved(currentLayout:Layout, cardChoice:Card, pileChoice:Int,
                             dcPileCards:(Card,Card), dcPileLengths:(Int,Int))
  case class SpitPile(winnersPileChoice:Int, deckLength:Int, player:ActorRef)
  case object LoserGiveMeLayout
  case class LosersLayout(layout:Layout)

  def log(receiver:String):PartialFunction[Any, Any] = {
    case s:StartRound =>
      println(s"\nINITIAL DEAL\n" +
        s"Player 1\n${s.player1Pile.map{card=>cardToString(card)}.mkString(" ")}\n" +
        s"Player 2\n${s.player2Pile.map{card=>cardToString(card)}.mkString(" ")}\n")
      s
    case d:DealCards =>
      println(s"\nDealCards\n" +
        s"Player $receiver layout: ${layoutToString(d.newLayout)}")
      d
    case r:ReadyToPlay =>
      println(s"\nReadyToPlay\n" +
        s"             says player ${r.player.path.name}")
      r
    case d:DeckTail =>
      d
    case m:PlayCards =>
      m.messageID match {
        case " " => // do nothing.
        case "TurnCard" =>
          println(s"\nPlayCards (Dealer --> Player $receiver)\n" +
            s"Player $receiver turns over ${cardToString(m.cardAndInt._1)}\n" +
            s"Player $receiver has ${m.cardAndInt._2} cards in their deck")
        case "DiscardApproved" =>
          println(s"\nPlayCards (DiscardApproved --> Player $receiver)       " +
            s"${cardToString(m.cardAndInt._1)}--->[${m.cardAndInt._2}]" +
            s"${dcPilesToString(m.dcPileCards,m.dcPileLengths)}\n" +
            s"                                          " +
            s"Old: ${layoutToString(m.compareLayouts._1)}\n" +
            s"                                          " +
            s"New: ${layoutToString(m.compareLayouts._2)}")
        case "MovedCards" =>

          println(s"\nPlayCards (Player $receiver moved cards ${m.moveCount} times)\n" +
            s"                                          " +
            s"Old: ${layoutToString(m.compareLayouts._1)}\n" +
            s"                                          " +
            s"New: ${layoutToString(m.compareLayouts._2)}")
        case _ => println("\n\n\nUnhandled case (PlayCards)\n\n\n")
      }
      m
    case d:DiscardCard =>
      d
    case c:CheckDiscard =>
      c
    case d:DiscardApproved =>
      d
    case s:SpitPile =>
      println(s"\nSpitPile\n" +
        s"                                             " +
        s"Discard Pile ${s.winnersPileChoice} was spat on by player ${s.player.path.name}!")
      s
    case LoserGiveMeLayout =>
      println(s"\nLoserGiveMeLayout\n" +
        s"Player $receiver lost! Layout handed in for new round...")
      LoserGiveMeLayout
    case m =>
      println(/*"\n\nUnhandled case! " + */receiver + " received " + m)
      m
  }
  /**
    * Dealer Actor
    * The dealer keeps 3 main mutable states, the discard piles, the stuck states of each player and the spitstate.
    * Methods below the receive function are also further separated into Unit and non-Unit methods.
    ************************************************************************************************/
  class Dealer extends Actor {
    var DiscardPile1:Pile = Nil
    var DiscardPile2:Pile = Nil
    var Player1Stuck = false
    var Player2Stuck = false
    var SpitState = false
// FIXME Debug messages
/*    println(s"\n\nDealer\n" +
      s"DCP1 Head: ${cardToString(DiscardPile1.headOption.flatten)}\n" +
      s"${DiscardPile1.map{card=>cardToString(card)}.mkString(" ")}\n" +
      s"DCP2 Head: ${cardToString(DiscardPile2.headOption.flatten)}\n" +
      s"${DiscardPile2.map{card=>cardToString(card)}.mkString(" ")}\n")
    system.terminate()/**/*/

    def receive = log("Dealer") andThen {

      // Start a round, when cards need to be collected and reshuffled.
      case StartRound(player1Pile, player2Pile) =>
        startRound(player1Pile, player2Pile)

        // Reports a ready state as well as a stuck state to the dealer.
      case ReadyToPlay(player) =>
        readyToPlay(player, (DiscardPile1,DiscardPile2))

        // The main message to confirm with the dealer that a given move is valied
      case CheckDiscard(cardChoice, pileChoice, currentLayout) =>
        checkDiscard(sender, cardChoice, pileChoice, currentLayout, (DiscardPile1, DiscardPile2))

        // This simulates a player spitting in the game.
      case SpitPile(winnersPileChoice, deckLength, player) =>
        spitPile(winnersPileChoice, deckLength, player)

    }
    // Cards are created, shuffled, split into two piles (possibly unequal) then sends these lists
    // to each player to store in their states.
    def startRound(player1Pile:Pile, player2Pile:Pile):Unit = {
      DiscardPile1 = Nil
      DiscardPile2 = Nil
      val ((p1newDeck, p1newLayout), (p2newDeck, p2newLayout)) = dealCards(player1Pile, player2Pile)
      player1 ! DealCards(p1newDeck, p1newLayout)
      player2 ! DealCards(p2newDeck, p2newLayout)
    }
    // This records each stuck state and progresses the game when the dealer finds that both players are waiting
    def readyToPlay(player:ActorRef, dcPiles:(Pile,Pile)):Unit = {
      def turnCard:Unit = {
        val beheadDeck1 = player1 ? DeckTail(None, 0, (dcPiles._1.length, dcPiles._2.length))
        beheadDeck1.foreach{

          case p1:DeckTail =>
            val beheadDeck2 = player2 ? DeckTail(None, 0, (dcPiles._1.length, dcPiles._2.length))
            beheadDeck2.foreach{

              case p2:DeckTail =>
                val updatedPile1 = p1.deckHead :: dcPiles._1
                val updatedPile2 = p2.deckHead :: dcPiles._2
                DiscardPile1 = updatedPile1
                DiscardPile2 = updatedPile2

                player1 ! PlayCards("TurnCard", 0, (Nil,Nil),
                  (updatedPile1.head, updatedPile2.head),
                  (updatedPile1.length, updatedPile2.length),
                  (p1.deckHead, p1.deckLength))
                player2 ! PlayCards("TurnCard", 0, (Nil,Nil),
                  (updatedPile1.head, updatedPile2.head),
                  (updatedPile1.length, updatedPile2.length),
                  (p2.deckHead, p2.deckLength))

              case x => x // If Deck is empty timeout.
            }
          case x => x // If Deck is empty timeout.
        }
      }
      if (player == player1) Player1Stuck = true
      if (player == player2) Player2Stuck = true
      if (Player1Stuck && Player2Stuck) {
        Player1Stuck = false
        Player2Stuck = false
        turnCard
      }
    }

    // This method is split into two main parts, one for each discard pile
    def checkDiscard(player:ActorRef,
                     cardChoice:Card,
                     pileChoice:Int,
                     currentLayout:Layout,
                     dcPiles:(Pile,Pile)):Unit = {

      val otherPlayer = if (player == player1) player2 else player1

      pileChoice match {
        case 1 =>
          // This creates a list of ints used to confirm valid card
          // choices that are available in a players layout.
          // If card numbers are in the list, update the discard pile.
          val validCardNumsForPile = validCardNums(dcPiles._1.headOption.flatten)
          if (validCardNumsForPile.contains(cardChoice.get._1)) {
            val updatedDcPile = cardChoice :: dcPiles._1

            DiscardPile1 = updatedDcPile

            Player1Stuck = false
            Player2Stuck = false

            // Notify the success of the discard to the playing player while
            // also notifying the other player that new choices are available for a new move.
            player ! DiscardApproved(currentLayout, cardChoice, pileChoice,
              (cardChoice, dcPiles._2.headOption.flatten),
              (dcPiles._1.length, dcPiles._2.length))

            otherPlayer ! PlayCards(" ",
              0, (Nil,Nil),
              (cardChoice, dcPiles._2.headOption.flatten),
              (dcPiles._1.length, dcPiles._2.length),
              (None,0))
          }
          else {
            // If the card is not valid, because the player was beaten by the other
            // Then request the player try again with the new discard cards.
            player ! PlayCards(" ",
              0, (Nil,Nil),
              (dcPiles._1.headOption.flatten, dcPiles._2.headOption.flatten),
              (dcPiles._1.length, dcPiles._2.length),
              (None,0))
          }
        case 2 =>
          val validCardNumsForPile = validCardNums(dcPiles._2.headOption.flatten)
          if (validCardNumsForPile.contains(cardChoice.get._1)) {
            val updatedDcPile = cardChoice :: dcPiles._2

            DiscardPile2 = updatedDcPile

            Player1Stuck = false
            Player2Stuck = false

            player ! DiscardApproved(currentLayout, cardChoice, pileChoice,
              (dcPiles._1.headOption.flatten, cardChoice),
              (dcPiles._1.length, dcPiles._2.length))

            otherPlayer ! PlayCards(" ",
              0, (Nil,Nil),
              (dcPiles._1.headOption.flatten, cardChoice),
              (dcPiles._1.length, dcPiles._2.length),
              (None,0))
          }
          else {
            player ! PlayCards(" ",
              0, (Nil,Nil),
              (dcPiles._1.headOption.flatten, dcPiles._2.headOption.flatten),
              (dcPiles._1.length, dcPiles._2.length),
              (None,0))
          }
      }
    }
    def spitPile(winnersPileChoice:Int, deckLength:Int, player:ActorRef):Unit = {
      if (SpitState) {
        // do nothing because the other player spat first.
      }
      else {
        // set the state immediately, simulates the slapping on the table.
        SpitState = true

        // an empty deck means the player has won, shut down the system after notifying.
        if (deckLength == 0) {
          println(s"                                    " +
            s"                                     " +
            s"Player ${player.path.name} has won the game!\n\n")
          system.stop(player1)
          system.stop(player2)
          system.terminate()
        }
        else {
          // for each player that won, the other losing player is notified for them to submit their layout
          // for reshuffling. The layout and decks are then mixed and submitted for a new round to start.
          if (player == player1) {
            val getLosersLayout = player2 ? LoserGiveMeLayout
            getLosersLayout.foreach{
              case loser:LosersLayout =>
                val p2LoserCards:Pile = loser.layout.flatMap{pile=>pile.map{card=>card}}
                if (winnersPileChoice == 1) dealer ! StartRound(DiscardPile1, p2LoserCards ++ DiscardPile2)
                else if (winnersPileChoice == 2)dealer ! StartRound(DiscardPile2, p2LoserCards ++ DiscardPile1)
                SpitState = false
            }
          }
          else if (player == player2) {
            val getLosersLayout = player1 ? LoserGiveMeLayout
            getLosersLayout.foreach{
              case loser:LosersLayout =>
                val p1LoserCards:Pile = loser.layout.flatMap{pile=>pile.map{card=>card}}
                if (winnersPileChoice == 1) dealer ! StartRound(p1LoserCards ++ DiscardPile2, DiscardPile1)
                else if (winnersPileChoice == 2)dealer ! StartRound(p1LoserCards ++ DiscardPile1, DiscardPile2)
                SpitState = false
            }
          }
        }
      }
    }
    /**
      * non-Unit methods
      * */
    // Takes each players piles and returns their deck and layout.
    // Used for the start of the game as well as subsequent rounds.
    def dealCards(player1Pile:Pile, player2Pile:Pile):((Pile, Layout),(Pile, Layout)) = {
      @tailrec
      def setPiles(count:Int, deck:Pile, layout:Layout):(Pile, Layout) = (count, deck) match {
        // If we have set the fifth pile, return the layout.
        // Note: We always want five piles (even if a pile is empty) to avoid exceptions
        case (6,_) => (deck, layout)
        case (c,Nil) =>
          setPiles(c + 1, Nil, Nil :: layout)
        case (c, d) =>
          val (pile, remainder) = d.splitAt(c)
          setPiles(c + 1, remainder, pile :: layout)
      }
      val p1Set = setPiles(1, player1Pile, Nil)
      val p2Set = setPiles(1, player2Pile, Nil)
      (p1Set, p2Set)
    }
  }
  /**
    * Player Actor.
    * Three mutable variables are kept; the deck, the layout, and how many times a player has moved their
    * layout around (to fill empty spots).*/
  class Player extends Actor {
    var Deck:Pile = Nil
    var PlayerLayout:Layout = Nil
    var MoveCount:Int = 0


    def receive = log(s"${self.path.name}") andThen {

      // Takes the dealt cards and sets them in their states.
      // The dealer is then notified that the player is ready
      case DealCards(newDeck, newLayout) =>
        setCards(Deck, newDeck, newLayout)

        // This requests the player to ".tail" their deck, i.e. to submit its head for turning it over.
      case DeckTail(_, _, dcPileLengths) =>
        deckTail(sender, Deck, dcPileLengths)

        // Main message used to activate a player to make a move.
      case PlayCards(_, moveCount, _, dcPileCards, dcPileLengths, _) =>

        playCards(MoveCount, PlayerLayout, dcPileCards, dcPileLengths, (None,0))


        // If a player has a valid choice, it is submitted to be discarded to the dealer.
      case DiscardCard(dcPileCards, dcPileLengths) =>
        discardCard(dcPileCards, dcPileLengths, PlayerLayout)

        // If the dealer approves the card, the player confirms this and is allowed to changes its layout.
      case DiscardApproved(currentLayout, cardChoice, pileChoice, dcPileCards, dcPileLengths) =>
        discardApproved(currentLayout, cardChoice, pileChoice, dcPileCards, dcPileLengths, Deck.length)

        // When a player loses a spit battle, their layout must be handed in for reshuffling.
      case LoserGiveMeLayout =>
        val senderD = sender
        val currentLayout = PlayerLayout
        senderD ! LosersLayout(currentLayout)

    }
    def playCards(moveCount:Int, currentLayout:Layout,
                  dcPileCards:(Card,Card), dcPileLengths:(Int,Int),
                  cardAndInt:(Card, Int)):Unit = {
      /*
      // FIXME This method is broken.
      val dupesStacked = stackDupes(currentLayout)*/
      // Try to fill any empty spaces with cards
      val movedLayout = fillEmptySpots(currentLayout)
      // Update the layout
      PlayerLayout = movedLayout
      // If fillEmptySpots produced no change (i.e. no spots can be filled), then move on to choose a card
      // Otherwise, try and move again if possible (moves are limited to 3 to prevent endless "valid" re-arranging)
      if (currentLayout == movedLayout || moveCount > 3) {
        self ! DiscardCard(dcPileCards, dcPileLengths)
        MoveCount = 0
      } // reset after log is able to print it.
      else {
        MoveCount += 1
        self ! PlayCards("MovedCards",
          MoveCount, (currentLayout, movedLayout),
          dcPileCards, dcPileLengths,
          (None, 0))
      }
    }

    def discardApproved(currentLayout:Layout, cardChoice:Card, pileChoice:Int,
                        dcPileCards:(Card,Card), dcPileLengths:(Int,Int), deckLength:Int):Unit = {

      // Update the layout by removing the one that was discarded.
      // This will also return the same layout if the card is not in the layout --
      // maybe the approve message was late and another card covers it or they submitted another
      // request involving it.
      val updatedLayout = updateLayoutAfterDiscard(cardChoice, currentLayout)
      PlayerLayout = updatedLayout

      // If the layout is empty, spit on the smaller pile.
      if (updatedLayout.forall{_ == Nil}) {
        val spitOnTheSmallerPile = if (dcPileLengths._1 < dcPileLengths._2) 1 else 2
        dealer ! SpitPile(spitOnTheSmallerPile, deckLength, self)
      }
        // if some layout cards remain, continue playing.
      else self ! PlayCards("DiscardApproved",
        0, (currentLayout, updatedLayout),
        dcPileCards,dcPileLengths,
        (cardChoice,pileChoice))
    }
    // This sequence determines which card is valid for playing and whether one exists in the current layout.
    // If there is a possible choice, play it, otherwise report to the dealer of being stuck.
    def discardCard(dcPileCards:(Card,Card), dcPileLengths:(Int,Int), currentLayout:Layout):Unit = {
      val (cardChoice, pileChoice) = chooseCard(dcPileCards, dcPileLengths, currentLayout)
      if (cardChoice.isDefined) dealer ! CheckDiscard(cardChoice, pileChoice, currentLayout)
      else dealer ! ReadyToPlay(self)
    }
    // Three cases when turning a card are handled to prevent NoElementExceptions for tail operations.
    def deckTail(senderD:ActorRef, oldDeck:Pile, dcPileLengths:(Int,Int)):Unit ={
      if (oldDeck.length > 1) {
        val newDeck = oldDeck.tail
        Deck = newDeck
        senderD ! DeckTail(oldDeck.head, newDeck.length, (0,0))
      }
      else if (oldDeck.length == 1) {
        Deck = Nil
        senderD ! DeckTail(oldDeck.head, 0, (0,0))
      }
      else {
        senderD ! _
      }
    }
    // set mutable variables.
    def setCards(oldDeck:Pile, newDeck:Pile, newLayout:Layout):Unit = {
      val updatedDeck = newDeck ++ oldDeck
      Deck = updatedDeck
      PlayerLayout = newLayout
      dealer ! ReadyToPlay(self)
    }
    /**
      * Non-Unit methods.
      *
      * ************************************************************************************************************/
    // Updates the layout if the requested card is still in the layout.
    // Removes a card by matching it to the layout then splicing and repairing.
    def updateLayoutAfterDiscard(cardToDiscard:Card, layout:Layout):Layout = {
      val layoutHeads = layout.map{pile=>pile.headOption}
      val pileToBeheadIndex = layoutHeads.indexOf(Some(cardToDiscard))
      if (pileToBeheadIndex < 0) layout
      else {
        val pileToBehead = layout(pileToBeheadIndex)
        val updatedPile = pileToBehead.tail
        val (layoutSplice, layoutSpliceToBehead) = layout.splitAt(pileToBeheadIndex)
        layoutSplice ++ (updatedPile :: layoutSpliceToBehead.tail)
      }

    }
    // This method will stack all the duplicate head cards into their own piles.
    // The result will be that all head cards are distinct.
    // If there are empty spots, this method will not fill them.
    // Unfortunately, there are some cases where this blows out the layout length.
    // There was no fix found for these edge cases so it has been commented out but left in
    // for reference.
    // FIXME This method is broken.
    def stackDupes(dupedLayout:Layout):Layout = {
      def listDupes(layout:Layout):List[Pile] = {
        val headCards:List[Card] = dupedLayout.flatMap{h=>h.headOption}
        headCards.map{ card =>
          headCards.filter{
            _.get._1 == card.get._1}}.filter{
          _.length > 1}.distinct
      }
      def loadPile(newLayout:Layout, dupes:Pile):Layout = {
        newLayout.foldRight(List.empty[Pile]){
          (pile, lay) => pile match {
            case Nil => Nil :: lay
            case p if p.head == dupes.head => (dupes ++ p.tail) :: lay
            case p if p.head.get._1 == dupes.head.get._1 => p.tail :: lay
            case p => p :: lay
          }
        }
      }
      @tailrec
      def loadPiles(newLayout:Layout, dupesList:Layout):Layout = dupesList match {
        case Nil => newLayout
        case _ =>
          loadPiles(loadPile(newLayout, dupesList.head), dupesList.tail)
      }

      // List the layout head cards
      val headCards = dupedLayout.flatMap{h=>h.headOption}

      val dupesList =                                 // List the duplicate head cards...
        headCards.map{ card =>                          // For every card in the list of layout head cards...
          headCards.filter{_.getOrElse((0,'\0'))._1 == card.getOrElse((0,'\0'))._1}       // List those cards with the same card number...
        }.filter{_.length > 1}.distinct                     // Make a distinct list of these list of cards.

      // If there are no duplicate cards return the original layout.
      // This is done to terminate a loop of self messaging in MoveCards.
      // Like in real life, one call of this method may reveal more dupes that can give
      // a new layout if this method is called again.
      if (dupesList.isEmpty) dupedLayout
      else loadPiles(dupedLayout, dupesList)
    }

    // This method attempts to fill empty spots by first checking if some piles contain duplicate
    // cards at the top. Designed to be used after stackDupes but still works on its own.
    def fillEmptySpots(spottedLayout:Layout):Layout = {
      // This method will split all the piles with duplicate cards (at the head -- because
      // the player shouldn't know what's underneath)
      // The empty spots will be filled with the piles of duplicate cards revealing what's underneath.
      // This needs dupesList.nonEmpty or NoHeadOfEmptyList exception is thrown
      def fillEmptyWithDupes(dupesCulled:Layout, dupesList:List[Pile]):Layout = (dupesCulled, dupesList) match {
        case (lay, Nil) => lay
        // If there is one pile left in the recursion but there are more than 1 dupe piles left to load,
        // just flatten all the remaining dupe piles then load them in the last layout pile.
        case (lay, dupes) if lay.length == 1 =>
          dupes.flatten ++ lay.head :: Nil
        // If the pile is empty, fill it with a dupe pile.
        case (lay, dupes) if lay.head.isEmpty =>
          dupes.head :: fillEmptyWithDupes(lay.tail, dupes.tail)
        case (lay, dupes) =>
          lay.head :: fillEmptyWithDupes(lay.tail, dupes)
      }

      // This method will fill all empty spots by splitting piles that have more than one card, and then
      // removing the empty lists.
      // This method will ignore the piles that have duplicates on top (splitting them will be useless)
      def fillEmptyAny(fillLayout:Layout, count:Int):Layout = (fillLayout, count) match {
        case (_, 0) => fillLayout
        case (Nil, _) => fillLayout
        case (lay, c) if lay.head.isEmpty =>
          lay.head :: fillEmptyAny(lay.tail, c)
        case (lay, c) if lay.head.length > 1 && lay.head.head.get._1 == lay.head.tail.head.get._1 =>
          lay.head :: fillEmptyAny(lay.tail, c)
        case (lay, c) if lay.head.length > 1 =>
          val splitPiles:Layout = List(List(lay.head.head)) :+ lay.head.tail
          splitPiles ++ fillEmptyAny(lay.tail, c - 1)
        case (lay, c) =>
          lay.head :: fillEmptyAny(lay.tail, c)
      }
      // dupesCulled will make a new layout where the piles that had duplicate cards are removed
      // and put into dupesList in preparation for fillEmptyWithDupes
      val (dupesCulled, dupesList) = spottedLayout.foldRight((List.empty[Pile],List.empty[Pile])){
        (pile, lay) => (pile, pile.length) match {
          case (p, l) if l <= 1 => (p :: lay._1, lay._2)
          case (p, _) if p.head.get._1 == p.tail.head.get._1 =>
            val (dupes, rest) = p.span{_.get._1 == p.head.get._1}
            (rest :: lay._1, dupes :: lay._2)
          case (p, _) => (p :: lay._1, lay._2)
        }
      }
      // Priority is given to fill the empty spots with piles of duplicate cards to maximize discard choices.
      // If there are no duplicate card piles, fill the empty spots with cards from any large pile.
      val emptyPilesCount = spottedLayout.count(_.isEmpty)
      if (emptyPilesCount != 0) {
        if (dupesList.nonEmpty) fillEmptyWithDupes(dupesCulled, dupesList)
        else fillEmptyAny(spottedLayout, emptyPilesCount).sortBy(_.isEmpty).take(5)
      }
      else spottedLayout
    }

    // This method will compare a list generated from validCardNums to determine if there are cards
    // in the current layout that match and are valid to discard on the currently known pile.
    def chooseCard(dcPileCards:(Card,Card), dcPileLengths:(Int,Int), currentLayout:Layout):(Card,Int) = {
      val biggerDcPile = if (dcPileLengths._1 > dcPileLengths._2) 1 else 2
      val (validPile1Nums, validPile2Nums) =
        (validCardNums(dcPileCards._1),
          validCardNums(dcPileCards._2))

      // Return the head cards in the layout. For empty piles, return nothing (i.e. don't return None.type)
      val layoutHeadCards = currentLayout.flatMap{ pile => pile.headOption }

      // Match the layout head cards with valid card numbers for the pile cards
      val validPile1Cards = layoutHeadCards.filter{card => validPile1Nums.contains(card.get._1)}
      val validPile2Cards = layoutHeadCards.filter{card => validPile2Nums.contains(card.get._1)}

      // If there is more than one option available for a given layout and pile card, choose randomly.
      def chooseRandomCard(cardOptions:Pile):Card = Random.shuffle(cardOptions).head

      // Handle the case where there are no options for a given layout and pile card.
      // If there's an option for only one pile, choose that pile
      // If there are options for both piles, choose the bigger pile to create a greater uneven distribution -> shorter game.
      // If there's no option at all, report none which leads to ReadyToPlay (stuck case).
      (validPile1Cards, validPile2Cards) match {
        case (Nil, Nil) => (None, 0)
        case (_, Nil) => (chooseRandomCard(validPile1Cards), 1)
        case (Nil, _) => (chooseRandomCard(validPile2Cards), 2)
        case (_, _) =>
          if (biggerDcPile == 1) (chooseRandomCard(validPile1Cards), 1)
          else (chooseRandomCard(validPile2Cards), 2)
      }
    }
  }
  // Instantiate actors
  val dealer = system.actorOf(Props[Dealer], name = "Dealer")
  val player1 = system.actorOf(Props[Player], name = "1")
  val player2 = system.actorOf(Props[Player], name = "2")

  def shuffleCards:(Pile, Pile) = {
    val packOfCards:Pile = for {
      suit <- List('c','d','h','s')
      number <- 1 to 13
    } yield Some((number, suit))
    Random.shuffle(packOfCards).splitAt(packOfCards.length / 2)
  }
  val (player1InitialPile, player2InitialPile) = shuffleCards
  dealer ! StartRound(player1InitialPile, player2InitialPile)
}

