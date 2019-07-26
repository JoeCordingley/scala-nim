package monads

trait Monad[F[_]] extends Applicative[F]{
  def flatMap[A,B](fa:F[A],f:A => F[B]):F[B]
  override def ap[A,B](fa:F[A], fab:F[A=>B]):F[B] = flatMap(fa, (a:A) => flatMap(fab,(f:A=>B) => pure(f(a))))
}
object Monad {
  def apply[F[_]:Monad]:Monad[F] = implicitly[Monad[F]]
  implicit class MonadOps[F[_]:Monad,A](fa:F[A]){
    def flatMap[B](f: A => F[B]):F[B] = Monad[F].flatMap(fa,f)
  }
}

object MonadLaws {
  import Monad._
  def leftIdentity[F[_]:Monad, A,B](a: A, f: A => F[B]):Boolean = (Monad[F].pure(a) flatMap f) == f(a)
  def rightIdentity[F[_]:Monad, A](fa:F[A]):Boolean = (fa flatMap Monad[F].pure) == fa
  def associativity[F[_]:Monad, A, B, C](fa:F[A],f:A => F[B], g:B => F[C]):Boolean = ((fa flatMap f) flatMap g ) == fa.flatMap(a => f(a) flatMap g)
}

object MonadInstances {
  type Id[A] = A
  import Monoid._
  import Monad._
  import Functor._
  implicit val identityMonad = new Monad[Id]{
    override def pure[A](a:A):A = a
    override def flatMap[A,B](a:A,f:A=>B):B = f(a)
  }
  implicit def readerMonad[R] = new Monad[Reader[R,?]]{
    override def pure[A](a:A):Reader[R,A] = Reader((_:R) => a)
    override def flatMap[A,B](fa:Reader[R,A],f:A=> Reader[R,B])= Reader[R,B](r => f(fa run r) run r )
  }
  implicit def writerMonad[L:Monoid] = new Monad[Writer[L,?]]{
    override def pure[A](a:A):Writer[L,A] = Writer(l = Monoid[L].empty, a = a)
    override def flatMap[A,B](fa:Writer[L,A],f:A=>Writer[L,B]):Writer[L,B] = {
      val newW = f(fa.a)
      Writer(l = fa.l |+| newW.l,newW.a)
    }
  }
  implicit def stateMonad[S] = new Monad[State[S,?]] {
    override def pure[A](a:A):State[S,A] = State((s:S) => (s,a))
    override def flatMap[A,B](fa:State[S,A],f:A=>State[S,B])= State[S,B]{ s1 =>
      val (s2,a) = fa.run(s1)
      f(a).run(s2)
    }
  }
  implicit val ioMonad = new Monad[IO]{
    override def pure[A](a:A):IO[A] = IO(() => a)
    override def flatMap[A,B](fa:IO[A],f:A=>IO[B]):IO[B] = IO(() =>f(fa.run()).run())
  }
  implicit val maybeMonad = new Monad[Maybe]{
    override def pure[A](a:A):Maybe[A] = Just(a)
    override def flatMap[A,B](fa:Maybe[A],f:A=>Maybe[B]):Maybe[B] = fa match {
      case Nowt() => Nowt()
      case Just(a) => f(a)
    }
  }
  type RNG[A] = State[Long,A]
  def nextLong:RNG[Long] = State{ seed =>
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    (newSeed,newSeed)
  }
  def nextInt:RNG[Int] = nextLong.map(l => (l >>> 16).toInt)
  def naturalNumber(max:Int):RNG[Int] = nextInt.map(_%(max + 1))
  def chooseInt(min:Int, max:Int):RNG[Int] = naturalNumber(max - min).map(_+min)
  type Gen[A] = RNG[A]

  
}

case class Reader[R,A](run:R=>A)
case class Writer[L,A](l:L,a:A)
object Writer {
  def write[L](l:L):Writer[L,Unit] = Writer(l=l,a=Unit)
}
case class State[S,A](run:S=>(S,A))
case class IO[A](run:()=>A)
sealed trait Maybe[A]
case class Nowt[A]() extends Maybe[A]
case class Just[A](a:A) extends Maybe[A]






