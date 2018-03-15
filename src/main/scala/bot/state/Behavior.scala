package bot.state

import bot._
import bot.pathfind.{Path, Pathfinder}

trait Behavior {
  val priorities = List(
    "seek-tavern",
    "seek-mine",
    "seek-hero",
    "idle"
  )

  def run(): Path
  def validate(): ValidReason
}

class SeekTavern(implicit val input: Input, implicit val board: PositionedBoard, implicit val pathfinder: Pathfinder) extends Behavior {

  val heroTile = PositionedTile(Tile.Hero(input.hero.id), input.hero.pos)

  override def run(): Path = {
    pathfinder.multiGoalFind(board.taverns, heroTile)
    

//    val mineObjective: Option[ScoredPos] = closestObjective(mines)
//    val tavernObjective: Option[ScoredPos] = closestObjective(taverns)
//    val heroObjective: Option[ScoredPos] = closestObjective(enemyHeroesPositions)
//    val closestHeroRef: Hero = input.bot.game.heroes filter {_.pos == heroObjective.get.objectivePos.pos} head
  }

  override def validate(): ValidReason = {
    val path: Path = pathfinder.multiGoalFind(board.taverns, heroTile)

    if (path.length == 1 && input.hero.life < 81) ValidReason(valid = true, "tavern path == 1 and hero life < 81")
    else if (input.hero.life < 98) ValidReason(valid = true, "hero life < 98")
    else ValidReason(valid = false)
    // (hero.life < requiredLifeToReachMine)
    //              // Head towards a tavern if there's no mine left to conquer
  }
}

class SeekHero(implicit val input: Input, implicit val board: PositionedBoard, implicit val pathfinder: Pathfinder) extends Behavior {
  override def run(): Path = {
    val hero = input.hero
    val enemyHeroesTiles: Vector[PositionedTile] = input.game.heroes.toVector collect {
      case h if h.id != hero.id => board at h.pos get
    }
    val heroTile = board at hero.pos get

    pathfinder.multiGoalFind(enemyHeroesTiles, heroTile)
  }

  override def validate(): ValidReason = ValidReason(valid = true, "always seek hero")
}

//val myHeroFilter: Hero => Boolean = (hero: Hero) => List("Ubick", "Liviu", "Oana").exists(hero.name ==)
//val startTime: Long = System.nanoTime
//val minLifeBeforeTavern = 41
//val hero: Hero = input.hero
//val board: Board = input.bot.game.board
//val positionedBoard: PositionedBoard = PositionedBoard(board.size, PositionedBoard.positionedTiles(board))
//val taverns: List[Pos] = positionedBoard.taverns
//val enemyHeroes: List[Hero] = input.bot.game.heroes filterNot myHeroFilter
//val mines: List[Pos] = positionedBoard.otherMinesPositions(enemyHeroes)
//val enemyHeroesPositions: List[Pos] = input.bot.game.heroes filterNot myHeroFilter map { _.pos }



//        val mineObjective: Option[ScoredPos] = closestObjective(mines)
//        val tavernObjective: Option[ScoredPos] = closestObjective(taverns)
//        val heroObjective: Option[ScoredPos] = closestObjective(enemyHeroesPositions)
//        val closestHeroRef: Hero = input.bot.game.heroes filter {_.pos == heroObjective.get.objectivePos.pos} head
//
//        if (enemyHeroesPositions.isEmpty) {
//          Stay
//        }// this might crash, but we should always have taverns
//        // If tavern is next to us and life is below 100, then move towards it
//        else if (tavernObjective.get.pathLength == 1 && hero.life < 95) {
//          moveTowardsClosestObjective(tavernObjective)
//        } else {
//          mineObjective match {
//              // Head towards a tavern if there's no mine left to conquer
//              case None => moveTowardsClosestObjective(tavernObjective)
//              case Some(sp) => {
//                // only attack heroes that are closer to us than the nearest mine and own at least 1 mine
//                if (heroObjective.get.pathLength <= 4 &&
//                  heroObjective.get.pathLength <= sp.pathLength &&
//                  positionedBoard.positionedTiles.exists(_.tile == Mine(Some(closestHeroRef.id))) &&
//                  closestHeroRef.pos != closestHeroRef.spawnPos
//                ) {
//                  if (hero.life > closestHeroRef.life + 21) {
//                    // Attack weak hero close to current position
//                    moveTowardsClosestObjective(heroObjective)
//                  } else {
//                    // Run away from strong hero close to current position
//
//                    val neighbors = hero.pos.neighbors filter {nb => heroObjective.get.nextPos.exists(_ != nb)}
//                    val escapeRoutes: Set[Pos] = neighbors.filter { p => positionedBoard.at(p).exists(t => t.tile == Air || t.tile == Tavern) }
//
//                    nextDir(escapeRoutes.headOption)
//                  }
//
//                } else {
//                  val requiredLifeToReachMine = minLifeBeforeTavern + sp.pathLength
//
//                  println(s"HP: ${hero.life} Life req for mine: $requiredLifeToReachMine")
//
//                  if (hero.life < requiredLifeToReachMine) {
//                    // Move towards tavern if life is insufficient to travel and conquer nearest tavern
//                    moveTowardsClosestObjective(tavernObjective)
//                  } else {
//                    // Move towards mine if life is sufficient
//                    moveTowardsClosestObjective(mineObjective)
//                  }
//              }
//            }
//          }
//        }
//      }