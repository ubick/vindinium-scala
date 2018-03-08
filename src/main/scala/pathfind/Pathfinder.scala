package pathfind

import bot.PositionedTile

object Pathfinder {
  def multiGoalFind(goals: Vector[PositionedTile], target: PositionedTile): Path = {
    // todo copy and re-implement multi-goal A*
    Path(Vector(target))
  }
}

case class Path(positionedTiles: Vector[PositionedTile]) {
  val length: Int = positionedTiles.length
  val next: Option[PositionedTile] = positionedTiles.lift(1)
}