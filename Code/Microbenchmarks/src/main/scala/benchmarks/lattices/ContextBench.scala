package benchmarks.lattices

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import rescala.core.Struct
import rescala.extra.lattices.dotstores.{Context, IntTree}
import rescala.extra.lattices.{IdUtil, Lattice}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class ContextBench[S <: Struct] {

  @Param(Array("1", "1000"))
  var setSize: Int = _

  var rep1Set: Context        = _
  var rep1SetPlusOne: Context = _
  var rep2Set: Context        = _
  val rep1id                  = IdUtil.genId()
  val rep2id                  = IdUtil.genId()
  var rep1single: Context     = _

  private def makeRep(rep: IdUtil.Id): Context = {
    Context(Map(rep -> IntTree.fromIterator((0 until setSize).iterator)))
  }

  @Setup
  def setup(): Unit = {
    rep1Set = makeRep(rep1id)
    rep2Set = makeRep(rep2id)
    rep1SetPlusOne = rep1Set.add(rep2id, 5)
    rep1single = Context.empty.add(rep1id, setSize + 10)
  }

  @Benchmark
  def merge() = Lattice.merge(rep1Set, rep2Set)

  @Benchmark
  def mergeSelf() = Lattice.merge(rep1Set, rep1Set)

  @Benchmark
  def mergeSelfPlusOne() = Lattice.merge(rep1Set, rep1Set)

  @Benchmark
  def diffSelf() = rep1Set.diff(rep1Set)

  @Benchmark
  def diffOther() = rep1Set.diff(rep2Set)

  @Benchmark
  def diffSingle() = rep1SetPlusOne.diff(rep1Set)

  @Benchmark
  def intersectSelf() = rep1Set.intersect(rep1Set)

  @Benchmark
  def intersectOther() = rep1Set.intersect(rep2Set)

}
