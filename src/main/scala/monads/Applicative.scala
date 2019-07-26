package monads
trait Applicative[F[_]] extends Functor[F] {
  import Applicative._
  def pure[A](a:A):F[A]
  def ap[A,B](fa:F[A], fab:F[A=>B]):F[B] 
  //= map2(fa, fab,(a:A,f:A=>B) => f(a))
  def map2[A,B,C](fa:F[A], fb:F[B], f:(A,B) => C):F[C] = ap(fb, ap(fa, pure(f.curried)))
  override def map[A,B](fa:F[A],f:A=>B):F[B] = ap(fa, pure(f))
}

object Applicative {
  def apply[F[_]:Applicative]:Applicative[F] = implicitly[Applicative[F]]
  implicit class ApplicativeOps[F[_]:Applicative,A](fa:F[A]){
    def ap[B](fab:F[A=>B]):F[B] = Applicative[F].ap(fa,fab)
    def map2[B,C](fb:F[B],f:(A,B) => C):F[C] = Applicative[F].map2(fa,fb,f)
  }
}

object ApplicativeLaws {
  import Applicative._
  def identityLaw[F[_]:Applicative,A](fa:F[A]):Boolean = (fa ap Applicative[F].pure(identity[A] _)) == fa
  def homomorphism[F[_],A,B](a:A, f:A=>B)(implicit m:Applicative[F]) = (m.pure(a) ap m.pure(f)) == m.pure(f(a))
  def interchange[F[_],A,B](a:A,fab:F[A=>B])(implicit m:Applicative[F]):Boolean = (m.pure(a) ap fab) == (fab ap m.pure((f:A=>B) => f(a)))
  def composition[F[_]: Applicative,A,B,C](fa:F[A],fab:F[A=>B], fbc:F[B=>C]):Boolean = ((fa ap fab) ap fbc) == (fa ap (fab ap (fbc ap Applicative[F].pure((bc: B => C) => (ab: A => B) => bc compose ab))))
}

object Validation {
  import MonoidInstances._
  import Monoid._
  sealed trait Validated[L,R]
  case class Valid[L,R](r:R) extends Validated[L, R]
  case class Invalid[L,R](l:L) extends Validated[L, R]
  implicit def validatedApplicative[L:Monoid]= new Applicative[Validated[L,?]]{
    override def pure[A](a:A):Validated[L,A] = Valid(a)
    override def map2[A,B,C](fa:Validated[L,A], fb:Validated[L,B], f:(A,B) =>C):Validated[L,C] = (fa, fb) match {
      case (Valid(a), Valid(b)) => Valid(f(a,b))
      case (Invalid(l), Invalid(m)) => Invalid(l |+| m)
      case (Invalid(l),_) => Invalid(l)
      case (_,Invalid(l)) => Invalid(l)
    }
    override def ap[A,B](fa:Validated[L,A], fab: Validated[L,A=>B]):Validated[L,B] = (fa, fab) match {
      case (Valid(a),Valid(f)) => Valid(f(a))
      case (Invalid(l), Invalid(m)) => Invalid(l |+| m)
      case (Invalid(l),_) => Invalid(l)
      case (_,Invalid(l)) => Invalid(l)
    }
  }
  val validated:Validated[List[String],Int] = Applicative[Validated[List[String],?]].map2[Int,Int,Int](Valid(1),Valid(2),_+_)

}
