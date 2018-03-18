package bot

import bot.Dir.Stay
import bot.Tile.{Air, Mine, Tavern, Wall}
import bot.pathfind.Pathfinder
import bot.state._

object GameEngine {
  case class GameContext(board: PositionedBoard, hero: Hero, enemyHeroes: Vector[Hero], pathfinder: Pathfinder)

  def play(input: Input): DirReason = {
    val hero: Hero = input.hero
    val simpleBoard: PositionedBoard = PositionedBoard(input.game.board.size, PositionedBoard.positionedTiles(input.game.board))

    // Start weighted tiles calc
    def positionedTileWeight(pt: PositionedTile): Int = {
      pt.tile match {
        case Tile.Hero(id) if id != hero.id && input.game.heroes.exists(h => h.id == id && hero.life - h.life > 20) => 30
        case _ if isPositionedTileNextToEnemy(pt) => 25
        case _ if isPositionedTileNextToEnemyNeighbor(pt) => 20
        case _ => 1
      }
    }
    def isPositionedTileNextToEnemyNeighbor(pt: PositionedTile): Boolean = {
      val neighborsOfNeighbors: Vector[Pos] = pt.pos.neighbors.toVector flatMap (_.neighbors) filter { pt.pos != } distinct

      neighborsOfNeighbors flatMap simpleBoard.at collectFirst {
        case PositionedTile(Tile.Hero(id), _, _) if id != hero.id => true
      } getOrElse false
    }

    def isPositionedTileNextToEnemy(pt: PositionedTile): Boolean = {
      pt.pos.neighbors.toVector flatMap simpleBoard.at collectFirst {
        case PositionedTile(Tile.Hero(id), _, _) if id != hero.id => true
      } getOrElse false
    }
    // End calc

    val board: PositionedBoard = simpleBoard.copy(positionedTiles =  simpleBoard.positionedTiles map { pt => pt.copy(weight = positionedTileWeight(pt))})

    def debugBoard(board: PositionedBoard): String = {
      def printPositionedTile(pt: PositionedTile): String = pt.tile match {
        case Wall => s"▓▓▓▓▓"
        case Air => s"  ${pt.weight}"
        case Mine(Some(id)) if id == hero.id => s"\u001B[32m  ◧ $id" + "\u001B[0m"
        case Mine(Some(id)) => s"\u001B[36m  ◧ $id" + "\u001B[0m"
        case Mine(None) => s"\u001B[36m  ◧  " + "\u001B[0m"
        case Tavern => s" \u001B[33m🍻 🍻 [0m"
        case Tile.Hero(id) if id == hero.id => s"  \u001B[32m😀  \u001B[0m"
        case Tile.Hero(id)=> s"  \u001B[31m😈 $id" + "\u001B[0m"
        case _ => s"?${pt.weight}"
      }

      board.positionedTiles.foldRight("") { (pt: PositionedTile, s: String) =>
        pt match {
          case PositionedTile(tile, pos, weight) =>
            val tileString = s + s"${printPositionedTile(pt).padTo(5, " ").mkString}"

            if (pos.y == board.size - 1) tileString + "\n"
            else tileString
          case _ => s + " ?? "
        }
      }
    }

    val enemyHeroes: Vector[Hero] = input.game.heroes.toVector filter ( _.id != input.hero.id )
    implicit val context: GameContext = GameContext(board, input.hero, enemyHeroes, new Pathfinder(input, board))

    val behaviors: List[Behavior] = List(
      new FleeHero(),
      new SeekHero(),
      new SeekMine(),
      new SeekTavern()
    )

    val dirReason: DirReason = behaviors collectFirst {
      case b if b.validate().valid => {
        val path = b.run()
        val pathDebug: String = s"${path.nextDir.toString.padTo(5, " ").mkString} | ${path.next} => ${path.destination}".replace("Some(", "").replace(")", "")
        val fullReason = s"$pathDebug - $b: ${b.validate().reason}"

        DirReason(path.nextDir, fullReason)
      }
    } getOrElse DirReason(Stay, "No behavior matched.")

    print("\033[H\033[2J")
    print(s"\nHero life: ${hero.life}\n")
    println(dirReason.reason)
    print(debugBoard(board))
    if (input.game.turn == input.game.maxTurns - 3) {
      print("\33[1A" * (board.size + 3))
    }

    dirReason
  }
}
