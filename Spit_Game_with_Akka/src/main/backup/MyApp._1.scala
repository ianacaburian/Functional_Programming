
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.util.{Failure, Random, Success}
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

object MyApp extends App {

  Console.println("As this extends App, the object's body is run at startup.")

  // This creates an Actor System
  val system = ActorSystem.apply("Spit")

  // The rest is over to you...

  type Card = (Int,Char)
  type Pile = List[Card]

  var DiscardPile1:Pile = Nil
  var DiscardPile2:Pile = Nil

  def cardToString(c:Card):String = c match{
    case (13,s) => s"K$s"
    case (12,s) => s"Q$s"
    case (11,s) => s"J$s"
    case (10,s) => s"T$s"
    case (1,s) => s"A$s"
    case (i,s) => s"$i$s"
  }
  def layoutToString(layout:List[Pile]):String = {
    def recurLayoutToString(count:Int, pile:Pile, string:String):String = (count, pile, string) match {
      case (c, Nil, s) => recurLayoutToString(count+1, layout(c), "_ " + s)
      case (5, p, s) => cardToString(p.head) + "."*p.tail.length + " " + s
      case (c, p, s) =>
        recurLayoutToString(count+1, layout(c), cardToString(p.head) + "."*p.tail.length + " " + s)
    }
    recurLayoutToString(1, layout.head, "")
  }

  implicit val timeout = Timeout(1.seconds)
  /*
  case class Fizz(i:Int)
  case class Buzz(i:Int)
  case class FizzBuzz(i:Int)
  case class Wrong[T](item:T)
  case class NextPlayerIs(a:ActorRef)
*/
  // FIXME change to objects where possible (for when params unnecessary)
  case object StartGame
  case class DealDeck(pile:Pile)
  case class LayoutPlay(card:Card, player:ActorRef)
  case class WhoWasFirst(discardPile:Pile, cardPlayed:Card)
  case class PileTopCard(card:Card, discardPile:ActorRef)
  case object Stuck
  case object Won


  def receiveLog(name:String):PartialFunction[Any, Any] = {
    case p =>
      println(name + " received " + p)
      p
  }
  class Dealer extends Actor {
    val packOfCards:Pile = for {
      s <- List('c','d','h', 's')
      n <- 1 to 13
    } yield (n, s)
    var ReadyPlayer1 = false
    var ReadyPlayer2 = false

    def receive = receiveLog(self.path.name) andThen {
      case StartRound =>
        val (deck1, deck2) = dealCards(packOfCards)
        val deck1Dealt = ask(player1, DealCards(deck1)).mapTo[Boolean]
        val deck2Dealt = ask(player2, DealCards(deck2)).mapTo[Boolean]
        deck1Dealt.foreach{ p1Ready =>
          ReadyPlayer1 = p1Ready
          deck2Dealt.foreach{
            p2Ready =>
              ReadyPlayer2 = p2Ready
              if (p1Ready && p2Ready) {

                player1 ! PlayDeck
                player2 ! PlayDeck
                println("BOTH PLAYERS READY.... GO!\n")
                ReadyPlayer1 = false
                ReadyPlayer2 = false
              }
          }
        }
      case WhoWasFirst(discardPile, cardPlayed) =>
        sender ! (discardPile.head == cardPlayed)

      case Stuck =>
        if (sender == player1) ReadyPlayer1 = false
        else ReadyPlayer2 = false
        if (ReadyPlayer1 == ReadyPlayer2) {
          player1 ! PlayDeck
          player2 ! PlayDeck
        }
      case Won =>
        if (sender == player1) {
          println(".........................................................Player 1 WON THE GAME")
          system.terminate()
        }
        else
          println(".........................................................Player 2 WON THE GAME")
          system.terminate()

    }
    def dealCards(pack: Pile): (Pile, Pile) = Random.shuffle(pack).splitAt(pack.length / 2)


  }
  class Player extends Actor {
    var Layout:List[Pile] = Nil
    var Deck:Pile = Nil

    def receive = receiveLog(self.path.name) andThen {

      case DealCards(p) =>
        val cards = p.map{x => cardToString(x)} mkString " "
        println("INITIAL DEAL\nPlayer " + self.path.name + "\n" + cards)
        setLayout(p)
        println("Player " + self.path.name + "'s Layout: " + layoutToString(Layout))
        println("Player " + self.path.name + " has " + Deck.length + " cards in their deck")
        if (self == player1) DiscardPile1 = Deck.head :: DiscardPile1
        else DiscardPile2 = Deck.head :: DiscardPile2
        println("Player " + self.path.name + " turns over " + cardToString(Deck.head))
        Deck = Deck.tail
        sender ! true

      case PlayDeck =>
        layoutPlay

    }
    def setLayout(deck:Pile) = {
      def setPiles(count:Int, deck:Pile, layout:List[Pile]):(Int, Pile, List[Pile]) = (count, deck) match {
        case (_,Nil) => (0, Nil, layout) // If there are no more cards, return the layout
        case (6,_) => (0, deck, layout)  // If we have set the fifth pile, return the layout
        case (c, d) =>
          val (pile, remainder) = d.splitAt(c)
          setPiles(count + 1, remainder, pile :: layout)
      }
      val (_, remainingDeck, newLayout) = setPiles(1, deck, Nil)
      Deck = remainingDeck
      Layout = newLayout
    }


    def layoutPlay = {
      def reLayout = ??? // FIXME make actors always rearrange optimal layout prior to making moves
      def validDiscards:(Pile,Pile) = {
        val it1 = Iterator.continually((1 to 13).toList).flatten
        val (it1prev, it1next) = it1.duplicate
        val disPile1Options = List(it1prev.drop(DiscardPile1.head._1-2).next,
                                    it1next.drop(DiscardPile1.head._1).next)

        val it2 = Iterator.continually((1 to 13).toList).flatten
        val (it2prev, it2next) = it2.duplicate
        val disPile2Options = List(it2prev.drop(DiscardPile2.head._1-2).next,
                                  it2next.drop(DiscardPile2.head._1).next)

        val layoutTopCards = Layout.map{p=>p.head}
        val validCardsForPile1 = disPile1Options.flatMap(i => layoutTopCards.filter(_._1 == i))
        val validCardsForPile2 = disPile2Options.flatMap(i => layoutTopCards.filter(_._1 == i))
        (validCardsForPile1, validCardsForPile2)
      }
/*      // Println statements to check validDiscard outputs correctly
      // Remember to check for DiscardPile == (13,_) and ensure same number/different suit combos are allowed
      println("\n\nPlayer " + self.path.name + " Discard Pile 1: " + DiscardPile1.head)
      println("Player " + self.path.name + " valid Cards For Pile 1: " + validDiscards._1)
      println("Player " + self.path.name + " Layout Top Cards: " + LayoutTopCards)

      println("\n\nPlayer " + self.path.name + " Discard Pile 2: " + DiscardPile2.head)
      println("Player " + self.path.name + " valid Cards For Pile 2: " + validDiscards._2)
      println("Player " + self.path.name + " Layout Top Cards: " + LayoutTopCards)
      //system.terminate()*/
      // FIXME Need to handle: when discard piles are empty (not to start), and if layout stuck
      val (focusDiscardPile1, focusDiscardPile2) = validDiscards
      val coinFlip = Math.random
      if (coinFlip > 0.5) focusDiscardPile1 match {
        case Nil => reLayout
        case cards =>
          val randomCard = Random.shuffle(cards).head

          println("Player " + self.path.name + " DiscardPile1 BEFORE: " + DiscardPile1.head)
          println("Player " + self.path.name + " Layout BEFORE: " + layoutToString(Layout))

          DiscardPile1 =  randomCard :: DiscardPile1

          val whoWon = ask(dealer, WhoWasFirst(DiscardPile1, randomCard)).mapTo[Boolean]
          whoWon.foreach{ iWon =>
            if (iWon) {
              Layout = Layout.map{p=>p.filterNot(_ == randomCard)}
              println("Player " + self.path.name + " WON")
              println("Player " + self.path.name + ", " + cardToString(randomCard) + " to discard 1. Layout: "
                + layoutToString(Layout))// FIXME assignment string: do not delete
            } else println("Player " + self.path.name + " LOST")
            println("Player " + self.path.name + " DiscardPile1 AFTER: " + DiscardPile1.head)
            println("Player " + self.path.name + " Layout AFTER: " + layoutToString(Layout))
          }
      } else focusDiscardPile2 match {
      case Nil => reLayout
      case cards =>
        val randomCard = Random.shuffle(cards).head

        println("Player " + self.path.name + " DiscardPile2 BEFORE: " + DiscardPile2.head)
        println("Player " + self.path.name + " Layout BEFORE: " + layoutToString(Layout))

        DiscardPile2 =  randomCard :: DiscardPile2

        val whoWon = ask(dealer, WhoWasFirst(DiscardPile2, randomCard)).mapTo[Boolean]
        whoWon.foreach{ iWon =>
          if (iWon) {
            Layout = Layout.map{p=>p.filterNot(_ == randomCard)}
            println("Player " + self.path.name + " WON")
            println("Player " + self.path.name + ", " + cardToString(randomCard) + " to discard 2. Layout: "
              + layoutToString(Layout))// FIXME assignment string: do not delete
          } else println("Player " + self.path.name + " LOST")
          println("Player " + self.path.name + " DiscardPile2 AFTER: " + DiscardPile2.head)
          println("Player " + self.path.name + " Layout AFTER: " + layoutToString(Layout))
        }
      }
      Thread.sleep(1000)
      system.terminate()
    }
  }

  // Create three of your players
  val dealer = system.actorOf(Props[Dealer], name = "Dealer")
  val player1 = system.actorOf(Props[Player], name = "1")
  val player2 = system.actorOf(Props[Player], name = "2")

  // Start the game by sending the first player the number 0
  dealer ! StartRound
}

