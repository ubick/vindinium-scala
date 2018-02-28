package bot

import bot.Dir._
import bot.Tile._

import scala.util.Random

trait Bot {
  def move(input: Input): Dir
}

class BasicBot extends Bot {
  val minLifeBeforeTavern = 30

  override def move(input: Input): Dir = {
    play(input)
  }

  def play(input: Input): Dir = {
    if (input.hero.life < minLifeBeforeTavern) {
      moveTowardsClosestTavern(input)
    } else {
      moveTowardsClosestMine(input)
    }
  }

  def isTileNotWalkable(t: Tile, input: Input) = {
    if (input.hero.life < minLifeBeforeTavern) t == Wall || t == Mine(Some(input.hero.id))
    else t == Tavern || t == Wall || t == Mine(Some(input.hero.id))
  }

  def moveTowardsClosestTavern(input: Input): Dir = {
    val board = input.game.board
    val positionedBoard: PositionedBoard = PositionedBoard(board.size, PositionedBoard.positionedTiles(board))
    val closestTavernPos: Pos = positionedBoard.closestTavernTo(input.hero.pos)

    pathFind(input, positionedBoard, input.hero.pos, closestTavernPos)
  }

  def moveTowardsClosestMine(input: Input): Dir = {
    val board = input.game.board
    val positionedBoard: PositionedBoard = PositionedBoard(board.size, PositionedBoard.positionedTiles(board))
    val otherMines: List[Pos] = positionedBoard.otherMines(input.hero.id)
    val closestMinePos: Pos = positionedBoard.closestPositionToCurrentPos(input.hero.pos, otherMines)

    pathFind(input, positionedBoard, input.hero.pos, closestMinePos)
  }

  def pathFind(input: Input, positionedBoard: PositionedBoard, start: Pos, end: Pos): Dir = {
    def walkablePositionsAroundPos(p: Pos): List[Pos] =
      p.neighbors.toList flatMap positionedBoard.at filterNot { pt => isTileNotWalkable(pt.tile, input) } map {
        _.pos
      }

    def scoredNeighbors(sp: ScoredPos, end: Pos): List[ScoredPos] = {
      val neighbors: List[Pos] = walkablePositionsAroundPos(sp.pos)

      neighbors map { p: Pos =>
        ScoredPos(p, 1, estimatedDistanceToTarget(p, end) + 1, Some(sp))
      }
    }

    case class ScoredPos(pos: Pos, g: Int, h: Int, parent: Option[ScoredPos] = None) {
      val f: Int = g + h
    }

    def estimatedDistanceToTarget(p1: Pos, p2: Pos) = Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y)

    def lowestFScore(list: List[ScoredPos]): ScoredPos = list match {
      case x :: y :: xs => if (x.f <= y.f) lowestFScore(x :: xs) else lowestFScore(y :: xs)
      case x :: Nil => x
    }

    // Implement A* search algorithm
    def shortestPathBetween(p1: Pos, end: Pos): Option[ScoredPos] = {
      val startPos = ScoredPos(p1, 0, estimatedDistanceToTarget(p1, end))
      val closedList: List[ScoredPos] = List(startPos)
      val openList: List[ScoredPos] = walkablePositionsAroundPos(p1) map { p =>
        ScoredPos(p, 1, estimatedDistanceToTarget(p, end) + 1, Some(startPos))
      }

      var i = 0

      def loop(open: List[ScoredPos], closed: List[ScoredPos]): Option[ScoredPos] = {
        if (open.isEmpty) {
          None
        } else {
          val s = lowestFScore(open)

          i = i + 1
          if (false) {
            Some(ScoredPos(input.hero.pos, 0, 0))
          } else {

            if (s.pos == end) Some(s)
            else {
              val newOpen = open.filterNot(_.pos == s.pos)
              val newClosed = s :: closed
              val neighbors: List[ScoredPos] = scoredNeighbors(s, end)

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

    def nextDir(pos: Option[ScoredPos]): Dir = {
      pos.map { p => {
        if (p.pos.y == input.hero.pos.y) {
          if (p.pos.x > input.hero.pos.x) South
          else North
        } else {
          if (p.pos.y > input.hero.pos.y) East
          else West
        }
      }
      }.getOrElse(Stay)
    }

    val heroShortestPathtoClosestMine = shortestPathBetween(start, end)

    nextDir(nextPos(heroShortestPathtoClosestMine))
  }

}

class RandomBot extends Bot {

  def move(input: Input) = {
    Random.shuffle(List(Dir.North, Dir.South, Dir.East, Dir.West)) find { dir ⇒
      input.game.board at input.hero.pos.to(dir) exists (Wall!=)
    }
  } getOrElse Dir.Stay
}
