import monix.execution.atomic.Atomic

case class Texture(name: String)

class Canvas {
  def draw(texture: Texture, x: Int, y: Int) = ???
}

object Texture {
  val empty: Texture = null

  val metal = Texture("metal")

  val spikes = Texture("spikes")
}

class Block(texture: Texture) {
  def render(delta: Long, canvas: Canvas, pos: BlockVector): Unit =
    if (texture != null) canvas.draw(texture, x, y)
}

object Block {

  object Air extends Block(Texture.empty)

  object Metal extends Block(Texture.metal)

  object Spikes extends Block(Texture.spikes)

}

class Entity {
  def update(delta: Long, world: World)

  def render(delta: Long, canvas: Canvas, world: World)
}

class Player extends Entity


object InputHandler {
  def isKeyPressed(key: String): Boolean = ???
}

case class Vector(x: Float, y: Float) {
  def +(vector: Vector) = Vector(x + vector.x, y + vector.y)
}

case class BlockVector(x: Int, y: Int) {
  def vector = Vector(x, y)
}

class World(size: BlockVector) {
  val map = Array.fill(size.x, size.y)(Block.Air)
  var entities = List.empty[(Entity, Atomic[Vector])]
  val player = new Player()

  def update(delta: Long): Unit = {
    for {
      col <- 0 until size.x
      row <- 0 until size.y
    } {
      val block = map(col)(row)
      block.update(delta, canvas, BlockVector(col, row))
    }

    for (entity <- entities) entity.update(delta, canvas)
  }

  def render(delta: Long, canvas: Canvas): Unit = {
    for {
      col <- 0 until size.x
      row <- 0 until size.y
    } {
      val block = map(col)(row)
      block.render(delta, canvas, BlockVector(col, row))
    }

    for (entity <- entities) entity.render(delta, canvas)
  }
}

object Game {
  var lastTime: Long = System.currentTimeMillis()
  val canvas = new Canvas()

  val world = new World()

  def move(entity: Entity, vec: Vector): Unit =
    entities.find(_._1 == entity).foreach(e => e._2 = e._2 + vec)

  def update(): Unit = {
    val now = System.currentTimeMillis()
    val delta = now - lastTime
    lastTime = now

    if (InputHandler.isKeyPressed("d"))
      move(player, Vector(1, 0))
    else if (InputHandler.isKeyPressed("a"))
      move(player, Vector(-1, 0))
    else if (InputHandler.isKeyPressed(" "))

    world.update(delta)

    render(delta)
  }

  def render(delta: Long): Unit = {
    world.render(delta, canvas)
  }
}
