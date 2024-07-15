package fr.esgi.al.funprog

import scala.annotation.tailrec

enum Mode:
  case 
    STREAMING, // the mowers are moving by the console input
    FULL // the mowers are moving by the input file present in config data

final class Area(val maxX: Int, val maxY: Int)
object Area {
  def apply(maxX: Int, maxY: Int): Area = {
    if(maxX < 0 || maxY < 0) new Area(5, 5) //todo remove
    else new Area(maxX, maxY)
  }
}


enum Orientation:
  case NORTH, EAST, SOUTH, WEST

enum Movement:
  case LEFT, RIGHT, FORWARD


final class Position(val x: Int, val y: Int)
object Position {
  def apply(x: Int, y: Int): Position = {
    if(x < 0 || y < 0) new Position(0, 0) //todo remove
    else new Position(x, y)
  }
}

final class ConfigData(val id: Int, val position: Position, val orientation: Orientation, val sequences: List[Movement])
final class ConfigGame(val name: String, val mode: Mode, val area: Area, val data: List[ConfigData])

final class Mower(val id: Int, val position: Position, val orientation: Orientation, val sequences: List[Movement]) {
  def move(movement: Movement): Mower = {
    movement match {
      case Movement.LEFT => new Mower(id, position, turnLeft(orientation), sequences)
      case Movement.RIGHT => new Mower(id, position, turnRight(orientation), sequences)
      case Movement.FORWARD => new Mower(id, moveForward(position, orientation), orientation, sequences)
    }
  }
  
  private def turnLeft(orientation: Orientation): Orientation = {
    orientation match {
      case Orientation.NORTH => Orientation.WEST
      case Orientation.EAST => Orientation.NORTH
      case Orientation.SOUTH => Orientation.EAST
      case Orientation.WEST => Orientation.SOUTH
    }
  }
  
  private def turnRight(orientation: Orientation): Orientation = {
    orientation match {
      case Orientation.NORTH => Orientation.EAST
      case Orientation.EAST => Orientation.SOUTH
      case Orientation.SOUTH => Orientation.WEST
      case Orientation.WEST => Orientation.NORTH
    }
  }
  
  private def moveForward(position: Position, orientation: Orientation): Position = {
    orientation match {
      case Orientation.NORTH => Position(position.x, position.y + 1)
      case Orientation.EAST => Position(position.x + 1, position.y)
      case Orientation.SOUTH => Position(position.x, position.y - 1)
      case Orientation.WEST => Position(position.x - 1, position.y)
    }
  }
}

final class Lawn(val area: Area, val mowers: List[Mower]) {
  @tailrec
  def move(mower: Mower, movements: List[Movement]): Mower = {
    movements match {
      case Nil => mower
      case head :: tail => move(mower.move(head), tail)
    }
  }
}

final class Game(val config: ConfigGame) {
  def play(): Unit = {
    config.mode match {
      case Mode.STREAMING => println("Streaming mode")
      case Mode.FULL => playFull()
    }
  }
  
  private def playFull(): Unit = {
    val mowers = config.data.map(data => new Mower(data.id, data.position, data.orientation, data.sequences))
    val lawn = new Lawn(config.area, mowers)
    val finalMowers = lawn.mowers.map(mower => lawn.move(mower, mower.sequences))
    finalMowers.foreach(mower => println(s"${mower.position.x} ${mower.position.y} ${mower.orientation}"))
  }
}

@main
def Main(): Unit = {
  val configGame = new ConfigGame("Game1", Mode.FULL, new Area(5, 5), List(
    new ConfigData(1, new Position(1, 2), Orientation.NORTH, List(Movement.LEFT, Movement.FORWARD, Movement.LEFT, Movement.FORWARD, Movement.LEFT, Movement.FORWARD, Movement.LEFT, Movement.FORWARD, Movement.FORWARD)),
    new ConfigData(2, new Position(3, 3), Orientation.EAST, List(Movement.FORWARD, Movement.FORWARD, Movement.RIGHT, Movement.FORWARD, Movement.FORWARD, Movement.RIGHT, Movement.FORWARD, Movement.RIGHT, Movement.RIGHT, Movement.FORWARD))
  ))

  val game = new Game(configGame)
  try {
    game.play()
  } catch {
    case e: Exception => println(e)
  }
}
