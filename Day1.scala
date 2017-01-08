import scala.language.higherKinds

// 1. CATEGORIES
// =============

trait Category[F[_,_]] {
  def identity[A] : F[A,A]
  def compose[A,B,C](f : F[B,C], g : F[A,B]) : F[A,C]
}

trait MonoidMorph[M,A,B] {
  def value : M
}

trait Monoid[M] {
  def empty : M
  def combine(m1 : M, m2 : M) : M
}

object Category {
  def fnCat = new Category[Function1] {
    def identity[A] = (x : A) => x
    def compose[A,B,C](f : B => C, g : A => B) = (x : A) => f(g(x))
  }

  def monoidCat[M](implicit mm : Monoid[M]) = new Category[({type t[A,B] = MonoidMorph[M,A,B]})#t] {
    def identity[A] = new MonoidMorph[M,A,A] { def value = mm.empty }
    def compose[A,B,C](f : MonoidMorph[M,B,C], g : MonoidMorph[M,A,B]) = new MonoidMorph[M,A,C] {
      def value = mm.combine(f.value, g.value) 
    }
  }
}


// 2. UNIVERSAL CONSTRUCTIONS
// ==========================

trait CategoryWithProducts[F[_,_], P[_,_]] extends Category[F] {
  def proj1[A,B] : F[P[A,B], A]
  def proj2[A,B] : F[P[A,B], B]
  def and[C,A,B](f : F[C,A], g : F[C,B]) : F[C, P[A,B]]
}

object CategoryWithProducts {
  def tupleCat = new CategoryWithProducts[Function1, Tuple2] {
    def identity[A] = Category.fnCat.identity
    def compose[A,B,C](f : B => C, g : A => B) = Category.fnCat.compose(f,g) 
    def proj1[A,B] = (x : (A,B)) => x._1
    def proj2[A,B] = (x : (A,B)) => x._2
    def and[C,A,B](f : C => A, g : C => B) = (x : C) => (f(x), g(x))
  }
}


// 3. DUALITY
// ==========

trait CategoryWithCoProducts[F[_,_], C[_,_]] extends Category[F] {
  def inj1[A,B] : F[A, C[A,B]]
  def inj2[A,B] : F[B, C[A,B]]
  def or[D,A,B](f : F[A,D], g : F[B,D]) : F[C[A,B], D]
}

case class Op[F[_,_],A,B](value : F[B,A])

object Op {
  def opCat[F[_,_]](implicit cc : Category[F]) = new Category[({type l[A,B] = Op[F,A,B]})#l] {
    def identity[A] : Op[F,A,A] = Op(cc.identity)
    def compose[A,B,C](f : Op[F,B,C], g : Op[F,A,B]) = (f,g) match {
      case (Op(f2), Op(g2)) => Op(cc.compose(g2, f2))
    }
  }
}


// 4. FUNCTORS
// ===========

trait Functor[F[_,_], G[_,_], O[_]] {
  def map[A,B](f : F[A,B]) : G[O[A],O[B]]
}

object Functor {
  def listFunctor = new Functor[Function1, Function1, List] {
    def map[A,B](f : A => B) = (x : List[A]) => x.map(f)
  }
}