package ex2

import scala.util.Random

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot, val batteryLevel: Double, val batteryUsagePerAction: Double) extends Robot:
  require(batteryLevel >= 0 && batteryUsagePerAction >= 0)
  private var remainingBattery = batteryLevel
  export robot.{position, direction, turn}
  override def act(): Unit = 
    if remainingBattery < batteryUsagePerAction then throw IllegalStateException("Robot ran out of battery")
    remainingBattery = remainingBattery - (batteryUsagePerAction)
    robot.act()

class RobotCanFail(val robot: Robot, val failChance: Double) extends Robot:
  require(0 <= failChance && failChance <= 1)
  var failed = false
  export robot.{position, direction, turn}
  override def act(): Unit =
    failed = failed || Random.nextDouble() <= failChance 
    if failed then throw IllegalStateException("Robot stopped working")
    robot.act()

class RobotRepeated(val robot: Robot, val repeats: Int) extends Robot:
  require(repeats >= 0)
  export robot.{position, direction, turn}
  override def act(): Unit =
    for _ <- (1 to repeats) do robot.act()

@main def testRobot(): Unit =
  val robot = RobotWithBattery(LoggingRobot(SimpleRobot((0, 0), Direction.North)), 1, 0.5)
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East
