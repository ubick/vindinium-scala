package bot.state

import bot.GameEngine.GameContext
import bot._
import bot.pathfind.{Path, Pathfinder}

class AssuredVictory(implicit val context: GameContext) extends Behavior {
  val hero: Hero = context.hero
  val board: PositionedBoard = context.board
  val pathfinder: Pathfinder = context.pathfinder
  val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)

  override def run(): Path = pathfinder.multiGoalFind(board.taverns, heroTile)

  override def validate(): ValidReason = {
    if (hero.mineCount > board.mines.length / 2) ValidReason(valid = true, "Owning 50% of all mines. Camping at the pub.")
    else ValidReason(valid = false)
  }

  override def toString: String = "Assured Victory"
}
