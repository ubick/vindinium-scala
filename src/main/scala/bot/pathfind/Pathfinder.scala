package bot.pathfind

import bot.Dir._
import bot.Tile.Air
import bot._

import scala.annotation.tailrec

class Pathfinder(input: Input, board: PositionedBoard) {
  val hero: Hero = input.hero

  def multiGoalFind(goals: Vector[PositionedTile], target: PositionedTile): Path = {
//    println(s"target ==\n $target")
//    println("neighbors \n")
//    isPositionedTileNextToEnemyNeighbor(target)

    def handleBestTile(best: ScoredTile, open: Vector[ScoredTile], closed: Vector[ScoredTile]): Option[ScoredTile] = {
      if (best.positionedTile.pos == target.pos) Some(best)
      else {
        val openWithoutBest: Vector[ScoredTile] = open.filterNot(_.positionedTile == best.positionedTile)
        val bestNeighbors: Vector[ScoredTile] = neighbors(best, target).filterNot(scoredTileIn(_, closed))
        val updatedOpenList: Vector[ScoredTile] = bestNeighbors.foldRight(openWithoutBest)(replaceTileWithLowerFScoreIn)
        val updatedClosedList: Vector[ScoredTile] = best +: closed

        lowestFScore(updatedOpenList).flatMap(handleBestTile(_, updatedOpenList, updatedClosedList))
      }
    }

    val openList: Vector[ScoredTile] = toScoredTiles(goals, target)
    Path.fromOptionScoredPos(lowestFScore(openList).flatMap(handleBestTile(_, openList, Vector.empty)))
  }

  private def scoredTileIn(st: ScoredTile, vector: Vector[ScoredTile]): Boolean = vector.exists(_.positionedTile == st.positionedTile)

  private def replaceTileWithLowerFScoreIn(st: ScoredTile, vector: Vector[ScoredTile]): Vector[ScoredTile] = vector match {
    case x +: xs if x.positionedTile == st.positionedTile && st.f < x.f => st +: xs
    case _ => st +: vector
  }

  private def lowestFScore(scoredTiles: Vector[ScoredTile]): Option[ScoredTile] = scoredTiles match {
    case x +: y +: xs => if (x.f <= y.f) lowestFScore(x +: xs) else lowestFScore(y +: xs)
    case x +: xs => Some(x)
    case _ => None
  }

  private def toScoredTiles(pts: Vector[PositionedTile], target: PositionedTile): Vector[ScoredTile] =
    pts map { st => ScoredTile(st, 1, Pathfinder.distanceHeuristic(st, target), None) }

  private def neighbors(source: ScoredTile, target: PositionedTile): Vector[ScoredTile] = {
    for {
      neighbor <- source.positionedTile.pos.neighbors.toVector flatMap board.at
      if Pathfinder.isTileWalkable(neighbor, target, hero)
    } yield ScoredTile(neighbor, source.g + neighbor.weight, Pathfinder.distanceHeuristic(neighbor, target), Some(source))
  }
}

object Pathfinder {
  def distanceHeuristic(pt1: PositionedTile, pt2: PositionedTile): Int =
    Math.abs(pt1.pos.x - pt2.pos.x) + Math.abs(pt1.pos.y - pt2.pos.y)

  def isTileWalkable(pt: PositionedTile, target: PositionedTile, hero: Hero): Boolean =
    if (pt.equals(target)) true
    else if (pt.tile.equals(Tile.Hero(hero.id))) true // allows flee paths to be calculated if they go through our hero
    else pt.tile.equals(Air)
}

case class ScoredTile(positionedTile: PositionedTile, g: Int, h: Int, parent: Option[ScoredTile]) {
  val f: Int = g + h
}

case class Path(positionedTiles: Vector[PositionedTile]) {
  val length: Int = if (positionedTiles.nonEmpty) positionedTiles.length - 1 else 0
  val current: Option[PositionedTile] = positionedTiles.lift(0)
  val next: Option[PositionedTile] = positionedTiles.lift(1)
  val destination: Option[PositionedTile] = positionedTiles.lastOption
  val nextDir: Dir = {
    val dir = for {
      currentTile <- current
      nextTile <- next
      d <- dirFromTiles(currentTile, nextTile)
    } yield d

    dir getOrElse Stay
  }
  val isEmpty: Boolean = positionedTiles.isEmpty
  val isNonEmpty: Boolean = positionedTiles.nonEmpty

  def dirFromTiles(p1: PositionedTile, p2: PositionedTile): Option[Dir] =
    Set(North, South, West, East) find { p1.pos.to(_) == p2.pos }
}

object Path {
  @tailrec
  def foldL[B](st: ScoredTile, z: B, f: (B, PositionedTile) => B): B = st match {
    case ScoredTile(positionedTile, _, _, Some(tile)) => foldL(tile, f(z, positionedTile), f)
    case ScoredTile(positionedTile, _, _, None) => f(z, positionedTile)
    case _ => z
  }

  def scoredPosToPositionedTiles(s: ScoredTile): Vector[PositionedTile] =
    foldL(s, Vector.empty, (b: Vector[PositionedTile], a) => a +: b).reverse

  def fromOptionScoredPos(o: Option[ScoredTile]): Path = Path(o.map(scoredPosToPositionedTiles).toVector.flatten)
}