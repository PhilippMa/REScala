package rescala.extra.lattices.delta

import rescala.extra.lattices.delta.CContext._
import rescala.extra.lattices.Lattice

trait DotStore[A] {
  def dots(ds: A): Set[Dot]

  def merge[C: CContext, D: CContext](left: A, leftContext: C, right: A, rightContext: D): (A, C)

  def bottom: A
}

object DotStore {
  def apply[A](implicit ds: DotStore[A]): DotStore[A] = ds

  def dots[A: DotStore](ds: A): Set[Dot] = DotStore[A].dots(ds)

  def merge[C: CContext, D: CContext, A: DotStore](left: A, leftContext: C, right: A, rightContext: D): (A, C) =
    DotStore[A].merge[C, D](left, leftContext, right, rightContext)

  def bottom[A: DotStore]: A = DotStore[A].bottom

  type DotSet = Set[Dot]
  implicit def DotSet: DotStore[Set[Dot]] = new DotStore[Set[Dot]] {
    override def dots(ds: Set[Dot]): Set[Dot] = ds

    override def merge[C: CContext, D: CContext](left: Set[Dot], leftContext: C, right: Set[Dot], rightContext: D): (Set[Dot], C) = {
      val inBoth = left intersect right
      val fromLeft = left.filter(!contains(rightContext, _))
      val fromRight = right.filter(!contains(leftContext, _))

      (inBoth union fromLeft union fromRight, union(leftContext, rightContext))
    }

    override def bottom: Set[Dot] = Set.empty[Dot]
  }

  type DotFun[A] = Map[Dot, A]
  implicit def DotFun[A: Lattice]: DotStore[Map[Dot, A]] = new DotStore[Map[Dot, A]] {
    override def dots(ds: Map[Dot, A]): Set[Dot] = ds.keySet

    override def merge[C: CContext, D: CContext](left: Map[Dot, A], leftContext: C, right: Map[Dot, A], rightContext: D): (Map[Dot, A], C) = {
      val inBoth = (left.keySet intersect right.keySet).map(dot => dot -> Lattice.merge(left(dot), right(dot))).toMap
      val fromLeft = left.filter { case (dot, _) => !contains(rightContext, dot) }
      val fromRight = right.filter { case (dot, _) => !contains(leftContext, dot) }

      (inBoth ++ fromLeft ++ fromRight, union(leftContext, rightContext))
    }

    override def bottom: Map[Dot, A] = Map.empty[Dot, A]
  }

  type DotMap[K, V] = Map[K, V]
  implicit def DotMap[K, V: DotStore]: DotStore[Map[K, V]] = new DotStore[Map[K, V]] {
    override def dots(ds: Map[K, V]): Set[Dot] = ds.values.flatMap(DotStore.dots(_)).toSet

    override def merge[C: CContext, D: CContext](left: Map[K, V], leftContext: C, right: Map[K, V], rightContext: D): (Map[K, V], C) = {
      val allKeys = left.keySet union right.keySet
      val mergedValues = allKeys map { k =>
        val (mergedVal, _) = DotStore.merge(left.getOrElse(k, DotStore[V].bottom), leftContext, right.getOrElse(k, DotStore[V].bottom), rightContext)
        k -> mergedVal
      } filter { case (_, v) => v != DotStore[V].bottom }

      (mergedValues.toMap, union(leftContext, rightContext))
    }

    override def bottom: Map[K, V] = Map.empty[K, V]
  }
}