package com.github.brendon.designpatterns.creational

import scala.collection.{mutable => mut}

sealed trait Direction
object Direction {
  case object North extends Direction
  case object South extends Direction
  case object East extends Direction
  case object West extends Direction
}

trait MapSite {def enter(): Unit}

class Wall extends MapSite {val enter = ???}

// The maze design example is non-ideal because there is a circular reference requirement embedded in the design
//  where a room must know about its doors and a door must know about its rooms.
// This leads to a problem where object references and immutability cannot both be maintained.  In order to keep
//   it consistent with the example, I've chosen to use a mutable map in the Room class.
class Room(val roomNumber: Int, val sides: mut.Map[Direction, MapSite] = mut.Map.empty) extends MapSite {
  def setSide(direction: Direction, mapSite: MapSite) = {
    sides(direction) = mapSite
    this
  }
  val enter = ???
}

class Door(room1: Room, room2: Room, isOpen: Boolean = false) extends MapSite {
  val rooms = room1 :: room2 :: Nil
  def otherSideFrom(room: Room): Option[Room] = rooms.find(_ != room)
  val enter = ???
}

case class Maze(rooms: Seq[Room] = Seq.empty) {
  def roomNo(no: Int) = rooms.find(_.roomNumber == no)
}
object Maze {
  def apply(room: Room, rooms: Room*): Maze = Maze(room +: rooms.toSeq)
}

// Enchanted
class Spell
class EnchantedRoom(roomNumber: Int, spell: Spell) extends Room(roomNumber)
class DoorNeedingSpell(room1: Room, room2: Room) extends Door(room1, room2)

// Bombed
class BombedWall extends Wall
class RoomWithABomb(n: Int) extends Room(n)

abstract class MazeFactory {
  def makeMaze: Maze = {
    import Direction._
    def aWall = makeWall

    val r1 = makeRoom(1)
    val r2 = makeRoom(2)
    val theDoor = makeDoor(r1, r2)

    r1.setSide(North, aWall)
      .setSide(East, theDoor)
      .setSide(South, aWall)
      .setSide(West, aWall)

    r2.setSide(North, aWall)
      .setSide(East, aWall)
      .setSide(South, aWall)
      .setSide(West, theDoor)

    Maze(r1, r2)
  }

  def makeWall: Wall = new Wall()
  def makeRoom(n: Int): Room = new Room(n)
  def makeDoor(room1: Room, room2: Room) = new Door(room1, room2)
}
object MazeFactory {

  object Default extends MazeFactory

  object Enchanted extends MazeFactory {
    override def makeRoom(n: Int) = new EnchantedRoom(n, new Spell)
    override def makeDoor(room1: Room, room2: Room) = new DoorNeedingSpell(room1, room2)
  }

  object Bombed extends MazeFactory {
    override def makeWall = new BombedWall()
    override def makeRoom(n: Int) = new RoomWithABomb(n)
  }
}

case class MazeConfig(mazeType: String) {
  def mazeFactory: MazeFactory = mazeType match {
    case "normal"    => MazeFactory.Default
    case "enchanted" => MazeFactory.Enchanted
    case "bombed"    => MazeFactory.Bombed
    case _           => throw new IllegalArgumentException(s"unknown mazeType $mazeType")
  }
}
object MazeConfig {
  def apply(args: Array[String]): MazeConfig = {
    require(args.length == 1, "must provide a maze type")
    MazeConfig(args(0))
  }
}

object MazeGame extends App {
  val config = MazeConfig(args)
  val factory = config.mazeFactory
  val maze = factory.makeMaze
}
