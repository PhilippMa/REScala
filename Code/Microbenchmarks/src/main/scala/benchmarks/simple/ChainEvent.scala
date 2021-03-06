package benchmarks.simple

import java.util.concurrent.TimeUnit

import benchmarks.{EngineParam, Size, Step, Workload}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams
import rescala.core.{Scheduler, Struct}
import rescala.interface.RescalaInterface
import rescala.reactives.{Event, Evt}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class ChainEvent[S <: Struct] {

  var engine: RescalaInterface[S]      = _
  implicit def scheduler: Scheduler[S] = engine.scheduler

  var source: Evt[Int, S]   = _
  var result: Event[Int, S] = _

  @Setup
  def setup(params: BenchmarkParams, size: Size, engineParam: EngineParam[S], work: Workload) = {
    engine = engineParam.engine
    source = engine.Evt[Int]()
    result = source
    for (_ <- Range(0, size.size)) {
      result = result.map { v =>
        val r = v + 1; work.consume(); r
      }
    }
  }

  @Benchmark
  def run(step: Step): Unit = source.fire(step.run())
}
