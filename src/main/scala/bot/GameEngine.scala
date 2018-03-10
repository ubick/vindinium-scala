package bot

import bot.Dir.Dir
import bot.pathfind.Pathfinder

object GameEngine {
  def play(input: Input): (Dir, String) = {
    val hero = input.hero
    val enemyHeroesTiles: Vector[PositionedTile] = input.game.heroes.toVector collect {
      case h if h.id != hero.id => PositionedTile(Tile.Hero(h.id), h.pos)
    }
    val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)
    val pathFinder = new Pathfinder(input)
    val path = pathFinder.multiGoalFind(enemyHeroesTiles, heroTile)
    val debug: String = s"${path.nextDir} | ${path.next} => ${path.destination}".replace("Some(", "").replace(")", "")

    (path.nextDir, debug)
  }
}
