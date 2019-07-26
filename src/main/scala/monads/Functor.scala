package monads


trait Functor[F[_]] {
  def map[A,B](fa:F[A], f: A => B):F[B]
}


object Functor {
  def apply[F[_]:Functor]:Functor[F] = implicitly[Functor[F]]
  implicit class FunctorOps[F[_]:Functor, A](fa: F[A]){
    def map[B](f:A => B):F[B] = Functor[F].map(fa,f)
  }
}

object FunctorLaws {
  import Functor._
  def identityLaw[F[_]:Functor,A](fa:F[A]):Boolean = (fa map identity) == fa
  def composition[F[_]:Functor,A,B,C](fa:F[A],f:B => C, g:A => B):Boolean = ((fa map g) map f) == (fa map (f compose g))
}

object ArbitraryFunctorInstances {
  import Functor._
  implicit def leftElementOfTwoTuple[A] = new Functor[(?,A)] {
    override def map[B,C](fa:(B,A),f:B => C):(C,A) = fa match {
      case (b,a) => (f(b),a)
    }
  }

  type StringTuple[A] = (A, String)
  val tuple:StringTuple[Int] = (5, "hey")
  val mapped:StringTuple[Double] = tuple.map(i => i.toDouble / 2)
}
