package fr.esgi.al.funprog

import scala.annotation.tailrec

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

final case class Mower(id: Int, position: Position, orientation: Orientation, sequences: List[Movement]) {
  def move(movement: Movement): Mower = {
    movement match {
      case Movement.LEFT => this.copy(orientation = turnLeft(orientation))
      case Movement.RIGHT => this.copy(orientation = turnRight(orientation))
      case Movement.FORWARD => this.copy(position = moveForward(position, orientation))
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

enum Mode:
  case FULL, STREAMING

final case class Config(name: String, mode: Mode)

final case class ConfigData(area: Area, mowers: List[Mower])
object ConfigData {
  def fromConsole(): (Area, List[Mower]) = {
    val input = scala.io.StdIn.readChar()
    //if i tape s
    if(input != 'S') {
      val area = Area(5, 5)
      val mowers = List(
        Mower(1, Position(1, 2), Orientation.NORTH, List(Movement.LEFT, Movement.FORWARD, Movement.LEFT, Movement.FORWARD, Movement.LEFT, Movement.FORWARD, Movement.LEFT, Movement.FORWARD, Movement.FORWARD)),
        Mower(2, Position(3, 3), Orientation.EAST, List(Movement.FORWARD, Movement.FORWARD, Movement.RIGHT, Movement.FORWARD, Movement.FORWARD, Movement.RIGHT, Movement.FORWARD, Movement.RIGHT, Movement.RIGHT, Movement.FORWARD))
      )
      (area, mowers)
    } else {
        val area = Area(5, 5)
        val mowers = Nil
        (area, mowers)

    }
  }
}

sealed trait GameMode {
  def name: String
  def run(): Report
}

final case class FullMode(name: String, configData: ConfigData) extends GameMode {
  override def run(): Report = {
    println("run full mode with name: " + name)
    val lawn = new Lawn(configData.area, configData.mowers)
    val mowers = configData.mowers.map(mower => lawn.move(mower, mower.sequences))
    mowers.foreach(mower => println(s"${mower.position.x} ${mower.position.y} ${mower.orientation}"))
    Report()
  }
}

final case class StreamingMode(name: String) extends GameMode {
  override def run(): Report = {
    val (initArea, initMowers) = ConfigData.fromConsole()
    val lawn = new Lawn(initArea, initMowers)
    val mowers = initMowers.map(mower => lawn.move(mower, mower.sequences))
    mowers.foreach(mower => println(s"${mower.position.x} ${mower.position.y} ${mower.orientation}"))

    val newLawn = new Lawn(initArea, mowers)
    runHelper(Some(newLawn))
  }

  @tailrec
  private def runHelper(lawn: Option[Lawn]): Report = {
    lawn match
      case Some(lawn) => {
        val (_, mowers) = ConfigData.fromConsole()
        if(mowers.isEmpty) Report()
        else {
          val newLawn = new Lawn(lawn.area, mowers)
          val newMowers = mowers.map(mower => newLawn.move(mower, mower.sequences))
          mowers.foreach(mower => println(s"${mower.position.x} ${mower.position.y} ${mower.orientation}"))

          val newNewLawn = new Lawn(lawn.area, newMowers)
          runHelper(Some(newNewLawn))
        }
      }
      case None => Report()
  }
}

final case class Report() {
    def exportTo(): Unit = {
        println("export report")
    }
}
final case class Program(config: Config) {
  def run(): Report = {
    println("run program with name: " + config.name)
    config.mode match {
      case Mode.FULL => {
        //val configData = ConfigData.fromInputFile()
        val configData = ConfigData(Area(5, 5), List(
          Mower(1, Position(1, 2), Orientation.NORTH, List(Movement.LEFT, Movement.FORWARD, Movement.LEFT, Movement.FORWARD, Movement.LEFT, Movement.FORWARD, Movement.LEFT, Movement.FORWARD, Movement.FORWARD)),
          Mower(2, Position(3, 3), Orientation.EAST, List(Movement.FORWARD, Movement.FORWARD, Movement.RIGHT, Movement.FORWARD, Movement.FORWARD, Movement.RIGHT, Movement.FORWARD, Movement.RIGHT, Movement.RIGHT, Movement.FORWARD))
        ))
        FullMode(config.name, configData).run()
      }
      case Mode.STREAMING => StreamingMode(config.name).run()
    }
  }
}

@main
def Main(): Unit = {
  //full
  val config = Config("config", Mode.FULL)
  val program = Program(config)

  val reporting = program.run()
  reporting.exportTo()
  
  //streaming
  val config2 = Config("config", Mode.STREAMING)
  val program2 = Program(config2)

  val reporting2 = program2.run()
  reporting2.exportTo()
}
