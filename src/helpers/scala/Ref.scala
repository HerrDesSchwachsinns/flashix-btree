package helpers.scala

class Ref[A](var get : A) {
  def := (x : A) { set(x) }
  def set(x : A) { get = x }
  override def equals(other : Any) = (get == other) 
  override def toString() = "Ref(" + (if(get!=null)get.toString else "null") +")"
}

object Ref {
  // NOTE: sollte im generierten Code nicht benötigt werden, verhindert Compilerfehler bei inkonsistent generiertem Code
  // implicit def toA[A](r : Ref[A]) : A = r.get
//  implicit def fromA[A](x : A) = new Ref(x)
  def empty[A] = new Ref[A](null.asInstanceOf[A])
}
