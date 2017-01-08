# Category Theory Through Functional Programming

## Day 1

### 1. Categories

* What is missing from a graph to make it a category?
  * How can you "fix" it? What do arrows represent?
  * Note: this construction is called the "free category from a graph"
* Describe other Categories
  * Types + isomorphisms
  * Types + lenses
* What special properties does the category of isomorphisms have? It is known as a "grupoid"

### 2. Universal Constructions

* Write the notion of initial and terminal objects as code
  * Prove that all initial objects are isomorphic, as done in the course
* What are the products, initial and terminal objects in...
  * A partial order seen as a category
  * Logic formulas, such as type classes
  * Hask, the category of types and functions
  * Hask*, the category of types with a designated element and functions which map the designated element from the source to the designated element in the target

### 3. Duality (advanced)

* Describe the opposite construction in code and prove it is a category

```
-- In Haskell:
newtype Op m a b = Op (m b a)
instance Category o m => Category o (Op m) where ...

// In Scala
case class Op[F[_,_],A,B](value : F[B,A])
object Op {
  def opCat[F[_,_]](implicit cc : Category[F]) = new Category[({type l[A,B] = Op[F,A,B]})#l] { ... }
}
```

* Prove, writing the code, that if a category has products, the opposite category has coproducts

### 4. Functors

* What is a functor between monoids seen as categories?
  * What is a functor between partial orders seen as categories?
* As a result of what we have explained, we can form a "big" category Cat where objects are categories themselves and arrows are functors between them. What are the identity functors?
* A generalization of the category Hask is the category of relations, Rel:
  * Objects are types
  * Arrows `r : A -> B` are functions `r : (A, B) -> Bool` which express whether an element of `A` and an element of `B` are related
  * Composition of `r : A -> B` and `s : B -> C` is defined such as `(s . r) (a, c) = True` if and only if there exists a `b` in `B` such that `r(a,b) = True` and `s(b,c) = True`
  * Verify this is a category
  * We say that "Rel is its own dual". In other words, there exists functors between Rel and the opposite of Rel. Can you build them?

### 5. Natural Transformations

* Write examples of natural transformations between
  * `List` and `Maybe`
  * `List` and `Const Int`
  * `- -> Bool` and `powerset(-)`

## Day 2

### 6. Monoidal Things

* Write instances of `Monoidal` for `[]` and `ZipList`
* We can have different monoidal structures in the same category. One example is that given by coproducts and the initial objects
  * How does this translate to Hask?
  * Write a type class describing a functor which translates from the product monoidal structure to the coproduct one
* Describe what a co-monoid is

### 7. (Co)Monads

* Define the monads for lists, `Maybe` (or any other you want) by their unit and multiplication
* Define the multiplication of the monad in terms of `>>=`/`bind` and viceversa
  * Hint: follow the types!
* Write the code for the Kleiski composition operator

### 8. Algebras

* Define natural numbers as initial algebras of the corresponding functor
* Do the same with binary trees

## Day 3

### 9. Adjunctions

* Write the code showing that coproducts are adjoint to the diagonal functor

### 10. Exponentials

### 11. (Co)Monads and Adjunctions

### 12. Yoneda

* Using Yoneda, compute the number of functions with the type `(Bool -> a) -> Maybe a` (taken from Bartosz Milewski's  blog)
* **Advanced**, write the functions witnessing:
  * Yoneda Lemma, that is,
    * `Functor k => forall b. ((a -> b) -> k b) -> k a`
    * `Functor k => forall b. k a -> (a -> b) -> k b`
  * Each natural transformation `forall x. (c -> x) -> (d -> x)` is given by a function `d -> c`
  * Objects are isomorphic if and only if the corresponding `A -> -` functors are

## Extra

### 13. (Co)Limits 