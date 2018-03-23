package bot.state

import bot.GameEngine.GameContext
import bot._
import bot.pathfind.{Path, Pathfinder}

class SeekHero(implicit val context: GameContext) extends Behavior {
  val hero: Hero = context.hero
  val board: PositionedBoard = context.board
  val pathfinder: Pathfinder = context.pathfinder
  val enemyHeroes: Vector[Hero] = context.enemyHeroes

  override def run(): Path = {
    val enemyHeroesTiles: Vector[PositionedTile] = board.positionedTiles collect {
      case PositionedTile(Tile.Hero(id), pos, _) if id != hero.id => board at pos get
    }
    val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)

    pathfinder.multiGoalFind(enemyHeroesTiles, heroTile)
  }

  override def validate(): ValidReason = {
    // filter tiles for enemy heroes that have at least 1 mine and their life is at least 21 HP less than ours
    val enemyHeroesTiles: Vector[PositionedTile] = enemyHeroes collect {
      case enemy if isEnemyAttackable(enemy) => board at enemy.pos get
    }

    val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)
    val path = pathfinder.multiGoalFind(enemyHeroesTiles, heroTile)

    if (path.isNonEmpty && path.length < 4) ValidReason(valid = true, s"Hero is ${path.length} tiles away.")
    else ValidReason(valid = false)
  }

  private def isEnemyAttackable(enemy: Hero): Boolean =
    hero.life - enemy.life > 20 &&
    enemy.mineCount > 0 &&
    enemy.pos != enemy.spawnPos

  override def toString: String = "Seek Hero"
}