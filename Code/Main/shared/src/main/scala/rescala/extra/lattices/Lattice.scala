package rescala.extra.lattices

/** Well, its technically a semilattice, but that is just more to type. */
trait Lattice[A] {

  /** By assumption: associative, commutative, idempotent. */
  def merge(left: A, right: A): A
}

object Lattice {
  def apply[A](implicit l: Lattice[A]): Lattice[A] = l

  def merge[A: Lattice](left: A, right: A): A = Lattice[A].merge(left, right)

  implicit class LatticeOps[A](val a: A) {
    def merge(other: A)(implicit l: Lattice[A]): A = l.merge(a, other)
  }

  implicit def SetAsLattice[A]: Lattice[Set[A]] =
    (left: Set[A], right: Set[A]) => left union right

  implicit def OptionAsLattice[A: Lattice]: Lattice[Option[A]] =
    (left: Option[A], right: Option[A]) => (left, right) match {
      case (None, r) => r
      case (l, None) => l
      case (Some(l), Some(r)) => Some(l merge r)
    }

  implicit def MapAsLattice[K, V: Lattice]: Lattice[Map[K, V]] =
    (left: Map[K, V], right: Map[K, V]) => {
      (left.keySet ++ right.keySet).flatMap { k =>
        (left.get(k) merge right.get(k)).map(k -> _)
      }.toMap
    }
}
