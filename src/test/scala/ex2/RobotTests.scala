package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotSpec extends AnyFlatSpec with Matchers:
  "A SimpleRobot" should "turn correctly" in:
    val robot = new SimpleRobot((0, 0), Direction.North)

    robot.turn(Direction.East)
    robot.direction should be(Direction.East)

    robot.turn(Direction.South)
    robot.direction should be(Direction.South)

    robot.turn(Direction.West)
    robot.direction should be(Direction.West)

    robot.turn(Direction.North)
    robot.direction should be(Direction.North)

  it should "act correctly" in:
    val robot = new SimpleRobot((0, 0), Direction.North)

    robot.act()
    robot.position should be((0, 1))

    robot.turn(Direction.East)
    robot.act()
    robot.position should be((1, 1))

    robot.turn(Direction.South)
    robot.act()
    robot.position should be((1, 0))

    robot.turn(Direction.West)
    robot.act()
    robot.position should be((0, 0))

  "A RobotWithBattery" should "move until its battery runs out" in:
    val robot = RobotWithBattery(SimpleRobot((0, 0), Direction.North), 1, 0.5)
    robot.act()
    robot.position should be((0, 1))
    robot.act()
    robot.position should be((0, 2))
    a [IllegalStateException] should be thrownBy robot.act()

  "A RobotCanFail" should "fail randomly" in:
    val robot = RobotCanFail(SimpleRobot((0, 0), Direction.North), 0.25)
    if robot.failChance > 0 then
      a [IllegalStateException] should be thrownBy:
        var pos = (0, 0) 
        while true do
          robot.act()
          pos = (pos._1, pos._2 + 1)
          robot.position should be(pos)

  "A RobotRepeated" should "repeat its actions N times" in:
    val repeats = 5
    val robot = RobotRepeated(SimpleRobot((0, 0), Direction.North), repeats)
    robot.act()
    robot.position should be((0, repeats))
