package scalaz

class UnionTypes {
  // https://stackoverflow.com/questions/3508077/how-to-define-type-disjunction-union-types
  type ¬[A] = A => Nothing
  type ∨[T, U] = ¬[¬[T] with ¬[U]]
  type ¬¬[A] = ¬[¬[A]]
  type |∨|[T, U] = { type λ[X] = ¬¬[X] <:< (T ∨ U) }


  type hasSize = {def size: Int} |∨| String

  def length[T : hasSize#λ](t : T) = t match {
    case s : String => s.length
    case l : {def size: Int} => l.size;
  }
}
