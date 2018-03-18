package bot.state

import bot._
import bot.pathfind.Path

trait Behavior {
  def run(): Path

  def validate(): ValidReason
}