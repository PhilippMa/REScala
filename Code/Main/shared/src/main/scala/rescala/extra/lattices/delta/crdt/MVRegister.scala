package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.Lattice
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.{CContext, DeltaCRDT, SetDelta}

object MVRegister {
  def apply[A: Lattice, C: CContext](replicaID: "a"): DeltaCRDT[DotFun[A], C] =
    DeltaCRDT(replicaID, DotFun[A].bottom, CContext[C].empty, List())

  def read[A]: DeltaQuery[DotFun[A], Set[A]] = df => df.values.toSet

  def write[A: Lattice](v: A): DeltaDotMutator[DotFun[A]] = (df, nextDot) =>
    SetDelta(Map(nextDot -> v), df.keySet + nextDot)

  def clear[A: Lattice]: DeltaMutator[DotFun[A]] = df =>
    SetDelta(DotFun[A].bottom, df.keySet)
}
