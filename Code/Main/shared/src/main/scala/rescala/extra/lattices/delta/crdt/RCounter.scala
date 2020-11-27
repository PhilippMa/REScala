package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.Lattice
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore._
import rescala.extra.lattices.delta.{CContext, DeltaCRDT, Dot, SetDelta}

object RCounter {
  implicit def IntPairAsLattice: Lattice[(Int, Int)] =
    (left: (Int, Int), right: (Int, Int)) => (left, right) match {
      case ((linc, ldec), (rinc, rdec)) =>
        if ((linc - ldec) > (rinc - rdec)) left else right
    }

  type Store = DotFun[(Int, Int)]

  def apply[C: CContext](replicaID: String): DeltaCRDT[Store, C] =
    DeltaCRDT(replicaID, DotFun[(Int, Int)].bottom, CContext[C].empty, List())

  def value: DeltaQuery[Store, Int] = df =>
    df.values.foldLeft(0) {
      case (counter, (inc, dec)) => counter + inc - dec
    }

  /**
   * Without using fresh, reset wins over concurrent increments/decrements
   * When using fresh after every time deltas are shipped to other replicas, increments/decrements win over concurrent resets
   */
  def fresh: DeltaDotMutator[Store] = (_, nextDot) =>
    SetDelta(DotFun[(Int, Int)].bottom + (nextDot -> ((0, 0))), Set(nextDot))

  private def update(u: (Int, Int)): DeltaDotMutator[Store] = (df, nextDot) => nextDot match {
    case Dot(replicaID, counter) =>
      val currentDot = Dot(replicaID, counter - 1)

      if (df.contains(currentDot)) {
        val newCounter = (df(currentDot), u) match {
          case ((linc, ldec), (rinc, rdec)) => (linc + rinc, ldec + rdec)
        }

        SetDelta(df + (currentDot -> newCounter), Set(currentDot))
      } else
        SetDelta(DotFun[(Int, Int)].bottom + (nextDot -> u), Set(nextDot))
  }

  def increment: DeltaDotMutator[Store] = update((1, 0))

  def decrement: DeltaDotMutator[Store] = update((0, 1))

  def reset: DeltaMutator[Store] = df => SetDelta(DotFun[(Int, Int)].bottom, df.keySet)
}
