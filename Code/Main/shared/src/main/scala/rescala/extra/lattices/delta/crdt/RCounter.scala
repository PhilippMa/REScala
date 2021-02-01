package rescala.extra.lattices.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rescala.extra.lattices.delta.DeltaCRDT._
import rescala.extra.lattices.delta.DotStore._
import rescala.extra.lattices.delta.{AntiEntropy, CContext, Causal, DeltaCRDT, Dot, UIJDLattice}

object RCounterCRDT {
  implicit def IntPairAsUIJDLattice: UIJDLattice[(Int, Int)] = new UIJDLattice[(Int, Int)] {
    override def leq(left: (Int, Int), right: (Int, Int)): Boolean = (left, right) match {
      case ((linc, ldec), (rinc, rdec)) =>
        linc <= rinc && ldec <= rdec
    }

    /**
     * Decomposes a lattice state into its unique irredundant join decomposition of join-irreducable states
     */
    override def decompose(state: (Int, Int)): Set[(Int, Int)] = state match {
      case (inc, dec) => Set((inc, 0), (0, dec))
    }

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: (Int, Int), right: (Int, Int)): (Int, Int) = (left, right) match {
      case ((linc, ldec), (rinc, rdec)) =>
        (linc max rinc, ldec max rdec)
    }

    override def bottom: (Int, Int) = (0, 0)
  }

  type State[C] = Causal[DotFun[(Int, Int)], C]

  def apply[C: CContext](antiEntropy: AntiEntropy[State[C]]): DeltaCRDT[State[C]] =
    DeltaCRDT.empty[State[C]](antiEntropy)

  def value[C: CContext]: DeltaQuery[State[C], Int] = {
    case Causal(df, _) =>
      df.values.foldLeft(0) {
        case (counter, (inc, dec)) => counter + inc - dec
      }
  }

  /**
   * Without using fresh, reset wins over concurrent increments/decrements
   * When using fresh after every time deltas are shipped to other replicas, increments/decrements win over concurrent resets
   */
  def fresh[C: CContext]: DeltaMutator[State[C]] = {
    case (replicaID, Causal(_, cc)) =>
      val nextDot = CContext[C].nextDot(cc, replicaID)

      Causal(
        DotFun[(Int, Int)].empty + (nextDot -> ((0, 0))),
        CContext[C].fromSet(Set(nextDot))
      )
  }

  private def update[C: CContext](u: (Int, Int)): DeltaMutator[State[C]] = {
    case (replicaID, Causal(df, cc)) =>
      CContext[C].max(cc, replicaID) match {
        case Some(currentDot) if df.contains(currentDot) =>
          val newCounter = (df(currentDot), u) match {
            case ((linc, ldec), (rinc, rdec)) => (linc + rinc, ldec + rdec)
          }

          Causal(
            df + (currentDot -> newCounter),
            CContext[C].fromSet(Set(currentDot))
          )
        case _ =>
          val nextDot = CContext[C].nextDot(cc, replicaID)

          Causal(
            DotFun[(Int, Int)].empty + (nextDot -> u),
            CContext[C].fromSet(Set(nextDot))
          )
      }
  }

  def increment[C: CContext]: DeltaMutator[State[C]] = update((1, 0))

  def decrement[C: CContext]: DeltaMutator[State[C]] = update((0, 1))

  def reset[C: CContext]: DeltaMutator[State[C]] = {
    case (_, Causal(df, _)) =>
      Causal(
        DotFun[(Int, Int)].empty,
        CContext[C].fromSet(df.keySet)
      )
  }
}

class RCounter[C: CContext](crdt: DeltaCRDT[RCounterCRDT.State[C]]) {
  def value: Int = crdt.query(RCounterCRDT.value)

  def fresh(): RCounter[C] = new RCounter(crdt.mutate(RCounterCRDT.fresh))

  def increment(): RCounter[C] = new RCounter(crdt.mutate(RCounterCRDT.increment))

  def decrement(): RCounter[C] = new RCounter(crdt.mutate(RCounterCRDT.decrement))

  def reset(): RCounter[C] = new RCounter(crdt.mutate(RCounterCRDT.reset))

  def processReceivedDeltas(): RCounter[C] = new RCounter(crdt.processReceivedDeltas())
}

object RCounter {
  type State[C] = RCounterCRDT.State[C]
  type Embedded = DotFun[(Int, Int)]

  def apply[C: CContext](antiEntropy: AntiEntropy[State[C]]): RCounter[C] =
    new RCounter(RCounterCRDT[C](antiEntropy))

  implicit def RCounterStateCodec[C: JsonValueCodec]: JsonValueCodec[Causal[Map[Dot, (Int, Int)], C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def RCounterEmbeddedCodec: JsonValueCodec[Map[Dot, (Int, Int)]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}
