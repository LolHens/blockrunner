import monix.execution.atomic.Atomic

import scala.io.Source

case class Texture(name: String) {
  private def newInputStream() = getClass.getResourceAsStream("textures/"+name)

  val data: Array[Byte] = {
    val inputStream = newInputStream()
    Stream.continually(inputStream.read).takeWhile(_ != -1).map(_.toByte).toArray
  }
}

class Canvas {
  def draw(texture: Texture, x: Int, y: Int): Unit = ???
}

object Texture {
  val empty: Texture = null

  val player = Texture("player")

  val metal = Texture("metal")

  val spikes = Texture("spike1s")
}

class Block(val texture: Texture,
            val pos: BlockVector) {
  def update(delta: Long) = ()

  def render(delta: Long, canvas: Canvas): Unit =
    if (texture != null) canvas.draw(texture, pos.x, pos.y)
}

object Block {

  case class Air(override val pos: BlockVector) extends Block(Texture.empty, pos)

  case class Metal(override val pos: BlockVector) extends Block(Texture.metal, pos)

  case class Spikes(override val pos: BlockVector) extends Block(Texture.spikes, pos)

}

abstract class Entity(var texture: Texture,
                      var world: World,
                      var pos: Vector,
                      var vel: Vector) {
  def update(delta: Long)

  def render(delta: Long, canvas: Canvas)
}

class Player(world: World, pos: Vector) extends Entity(Texture.player, world, pos, Vector.Null) {
  override def update(delta: Long): Unit = ???

  override def render(delta: Long, canvas: Canvas): Unit = ???
}


object InputHandler {
  def isKeyPressed(key: String): Boolean = ???
}

case class Vector(x: Float, y: Float) {
  def +(vector: Vector) = Vector(x + vector.x, y + vector.y)
}

object Vector {
  val Null: Vector = Vector(0, 0)
}

case class BlockVector(x: Int, y: Int) {
  def vector = Vector(x, y)
}

class World(val size: BlockVector, playerPos: Vector) {
  object Map {
    private val map: Array[Array[Block]] = Array.tabulate(size.x, size.y)((x, y) => Block.Air(BlockVector(x, y)))

    def apply(pos: BlockVector): Block = map(pos.x)(pos.y)

    def +=(block: Block): Unit = map(block.pos.x)(block.pos.y) = block

    def ++=(blocks: Seq[Block]): Unit = blocks.foreach(this += _)

    def -=(block: Block): Unit =
      if (this(block.pos) == block)
        map(block.pos.x)(block.pos.y) = Block.Air(block.pos)

    def --=(blocks: Seq[Block]): Unit = blocks.foreach(this -= _)

    def list: Seq[Block] = for {
        col <- 0 until size.x
        row <- 0 until size.y
      } yield map(col)(row)
  }

  abstract class Physics {
    def update(delta: Long, entity: Entity): Unit
  }

  object Physics {
    case object None extends Physics {
      override def update(delta: Long, entity: Entity): Unit = ()
    }

    case object Gravity extends Physics {
      override def update(delta: Long, entity: Entity): Unit = {
        entity
      }
    }
  }

  object Entities {
    private var entities = List.empty[Entity]

    def +=(entity: Entity): Unit = entities = entity +: entities

    def ++=(entities: Seq[Entity]): Unit = entities.foreach(this += _)

    def -=(entity: Entity): Unit = entities = entities.filterNot(_ == entity)

    def --=(entities: Seq[Entity]): Unit = entities.foreach(this -= _)

    def list: Seq[Entity] = entities
  }

  val player = new Player(this, playerPos)

  def update(delta: Long): Unit = {
    for (block <- Map.list) block.update(delta)
    for (entity <- Entities.list) entity.update(delta)
  }

  def render(delta: Long, canvas: Canvas): Unit = {
    for (block <- Map.list) block.render(delta, canvas)
    for (entity <- Entities.list) entity.render(delta, canvas)
  }
}

object Game {
  var lastTime: Long = System.currentTimeMillis()
  val canvas = new Canvas()

  val world = new World(BlockVector(30, 20), Vector(10, 10))

  def update(): Unit = {
    val now = System.currentTimeMillis()
    val delta = now - lastTime
    lastTime = now

    if (InputHandler.isKeyPressed("d"))
      world.player.vel += Vector(1, 0)
    else if (InputHandler.isKeyPressed("a"))
      world.player.vel += Vector(-1, 0)
    else if (InputHandler.isKeyPressed(" "))
      world.player.vel += Vector(0, 1)

    world.update(delta)

    render(delta)
  }

  def render(delta: Long): Unit = {
    world.render(delta, canvas)
  }
}
