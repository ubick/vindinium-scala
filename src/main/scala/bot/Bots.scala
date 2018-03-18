package bot

import bot.Dir._

trait Bot {
  def move(input: Input): Dir
}

class BasicBot extends Bot {
  override def move(input: Input): Dir = {
    val startTime: Long = System.nanoTime
    val output = GameEngine.play(input)
    val ms: Double = Math.floor((System.nanoTime - startTime) * 1000 / 1e9d)
    val paddedMs = s"$ms".padTo(4, " ").mkString
    val paddedTurn = s"${input.game.turn}/${input.game.maxTurns}".padTo(7, " ").mkString

//    println(f"[$paddedMs ms] Turn [$paddedTurn] ${output.reason} ")

    output.dir
  }
}

