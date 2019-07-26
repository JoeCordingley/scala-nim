package monads

trait Monoid[A] {
  def empty:A
  def combine(x:A,y:A):A
}
object Monoid {
  def apply[A: Monoid] = implicitly[Monoid[A]]
  implicit class MonoidOps[A:Monoid](x:A){
    def |+|(y:A):A = Monoid[A].combine(x,y)
  }
}

object MonoidLaws {
  import Monoid._
  def leftIdentity[A:Monoid](a:A):Boolean = (Monoid[A].empty |+| a) == a
  def rightIdentity[A:Monoid](a:A):Boolean = (a |+| Monoid[A].empty) == a
  def associativity[A:Monoid](x:A,y:A,z:A) =  ((x |+| y) |+| z) == (x |+| (y |+| z))
}
object MonoidInstances {
  import Monoid._
  implicit def list[A] = new Monoid[List[A]]{
    override def empty = Nil
    override def combine(x:List[A],y:List[A]) = x ::: y
  }
  val concatenated:List[Int] =  List(5,6) |+| Nil
}
