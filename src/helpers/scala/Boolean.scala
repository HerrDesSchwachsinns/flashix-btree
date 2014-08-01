package helpers.scala
import scala.util._

object Boolean 
{
  def uninit() : Boolean = Randomizer.random()

  implicit object Randomizer extends Randomizer[Boolean]
  {
    override def random() : Boolean = Random.generator.nextBoolean
  }
}
