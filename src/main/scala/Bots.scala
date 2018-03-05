package bot

import bot.Dir._
import bot.Tile._

import scala.util.Random

trait Bot {
  def move(input: Input): Dir
}

class BasicBot extends Bot {


  override def move(input: Input): Dir = {
    object Game {
      val startTime: Long = System.nanoTime
      val minLifeBeforeTavern = 25
      val hero: Hero = input.hero
      val board: Board = input.game.board
      val positionedBoard: PositionedBoard = PositionedBoard(board.size, PositionedBoard.positionedTiles(board))
      val taverns: List[Pos] = positionedBoard.taverns
      val mines: List[Pos] = positionedBoard.otherMines(input.game.heroes.filter(h => h.name == "Ubick" || h.name == "Liviu"))

      val play: Dir = {
        val closestMine: Option[ScoredPos] = closestObjective(mines)
        val closestTavern: Option[ScoredPos] = closestObjective(taverns)

        // this might crash, but we should always have taverns
        // If tavern is next to us and life is below 100, then move towards it
        println(s"Tavern path lentgh ${closestTavern.get.pathLength}")
        if (closestTavern.get.pathLength == 1 && hero.life < 95) {
          println("life is below 100, I should move to closest tavern")
          moveTowardsClosestObjective(closestTavern)
        } else {
          closestMine match {
            // Head towards a tavern if there's no mine left to conquer
            case None => moveTowardsClosestObjective(closestTavern)
            case Some(sp) => {
              val requiredLifeToReachMine = minLifeBeforeTavern + sp.pathLength

              if (hero.life < requiredLifeToReachMine) {
                // Move towards tavern if life is insufficient to travel and conquer nearest tavern
                moveTowardsClosestObjective(closestTavern)
              } else {
                // Move towards mine if life is sufficient
                nextDir(sp.nextPos)
              }
            }
          }
        }


      }

      def closestObjective(objectives: List[Pos]): Option[ScoredPos] = multiGoalPathFind(objectives, input.hero.pos)

      def nextDir(pos: Option[Pos]): Dir = {
        pos.map { p => {
          if (p.y == input.hero.pos.y) {
            if (p.x > input.hero.pos.x) South
            else North
          } else {
            if (p.y > input.hero.pos.y) East
            else West
          }
        }
        }.getOrElse(Stay)
      }

      def nextPos(path: Option[ScoredPos]): Option[ScoredPos] = {
        def loop(path: Option[ScoredPos]): Option[ScoredPos] = path match {
          case Some(p) => {
            val parent: Option[ScoredPos] = p.parent

            if (parent.exists(_.pos == input.hero.pos)) path
            else loop(parent)
          }
          case _ => None
        }

        loop(path)
      }

      def heroFromTile(t: Tile): Option[Hero] = t match {
        case Tile.Hero(id) => input.game.heroes.find(_.id == id)
        case _ => None
      }

      def isTileWalkable(t: Tile, destination: Pos): Boolean = t match {
        // Can't walk via walls and own mines
        case Wall | Mine(Some(hero.id)) => false
        // Don't attack mines while walking towards a tavern to restore health
        case Mine(_) if positionedBoard.at(destination) exists {
          _.tile == Tavern
        } => false
        // Avoid heroes if they have more life
        case Tile.Hero(_) if heroFromTile(t).exists(_.life >= hero.life + 1) => false
        // Camp at a tavern if there's no mine to capture
        case Tavern if mines.isEmpty => true
        // Avoid Taverns if life is max
        case Tavern if hero.life < 95 => false
        case _ => true
      }

      def moveTowardsClosestObjective(objective: Option[ScoredPos]): Dir = {
        val dir: Option[Dir] = for {
          o <- objective
          dir = nextDir(o.nextPos)
        } yield dir

        dir.getOrElse(Stay)
      }

      def estimatedDistanceToTarget(p1: Pos, p2: Pos): Int = Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y)

      def lowestFScore(list: List[ScoredPos]): ScoredPos = list match {
        case x :: y :: xs => if (x.f <= y.f) lowestFScore(x :: xs) else lowestFScore(y :: xs)
        case x :: Nil => x
      }

      def walkablePositionsAroundPos(p: Pos, target: Pos): List[Pos] =
        p.neighbors.toList flatMap positionedBoard.at filter { pt => isTileWalkable(pt.tile, target) } map {
          _.pos
        }

      def scoredNeighbors(sp: ScoredPos, end: Pos): List[ScoredPos] = {
        val neighbors: List[Pos] = walkablePositionsAroundPos(sp.pos, end)

        neighbors map { p: Pos =>
          ScoredPos(p, 1, estimatedDistanceToTarget(p, end) + 1, Some(sp))
        }
      }

      def multiGoalPathFind(goals: List[Pos], target: Pos): Option[ScoredPos] = {
        val openList: List[ScoredPos] = goals map { p =>
          ScoredPos(p, 1, estimatedDistanceToTarget(p, target) + 1, None)
        }
        val closedList: List[ScoredPos] = List()

        def loop(open: List[ScoredPos], closed: List[ScoredPos]): Option[ScoredPos] = {
          if (open.isEmpty) {
            None
          } else {
            val s = lowestFScore(open)

            if (false) {
              Some(ScoredPos(input.hero.pos, 0, 0))
            } else {
              if (s.pos == target) Some(s)
              else {
                val newOpen = open.filterNot(_.pos == s.pos)
                val newClosed = s :: closed
                val neighbors: List[ScoredPos] = scoredNeighbors(s, target)

                def parseNeighbors(neighbors: List[ScoredPos], openAcc: List[ScoredPos]): List[ScoredPos] = neighbors match {
                  case x :: xs if !closed.exists(_.pos == x.pos) =>
                    if (!openAcc.exists(_.pos == x.pos)) {
                      parseNeighbors(xs, x :: openAcc)
                    }
                    else {
                      val existingT: ScoredPos = openAcc.filter(_.pos == x.pos).head

                      if (x.f < existingT.f) {
                        val o = openAcc.map { i => if (i.pos == x.pos) x else i }

                        parseNeighbors(xs, o)
                      } else parseNeighbors(xs, openAcc)
                    }
                  case x :: xs => parseNeighbors(xs, openAcc)
                  case Nil => openAcc
                }

                val newNewOpen = parseNeighbors(neighbors, newOpen)

                loop(newNewOpen, newClosed)
              }
            }

          }
        }

        loop(openList, closedList)
      }

      val durationMilliseconds: Double = (System.nanoTime - startTime) * 1000 / 1e9d
      println(s"Took $durationMilliseconds ms to compute move")
    }

    if (Game.play == Stay) println("Decided to stay")

    Game.play

  }


}

class RandomBot extends Bot {

  def move(input: Input) = {
    Random.shuffle(List(Dir.North, Dir.South, Dir.East, Dir.West)) find { dir ⇒
      input.game.board at input.hero.pos.to(dir) exists (Wall !=)
    }
  } getOrElse Dir.Stay
}
