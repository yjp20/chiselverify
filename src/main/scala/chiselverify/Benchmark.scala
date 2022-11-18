package chiselverify

import chiselverify.Expr._

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental.sanitizeFileName
import chiseltest.formal.Formal
import chiseltest.internal.TestEnvInterface
import chiseltest.internal.BackendInstance
import chiseltest.internal.Context
import firrtl.options.TargetDirAnnotation

import org.openjdk.jmh.annotations.{Benchmark, Fork => JMHFork, Mode, BenchmarkMode, State, Scope, Setup, Level}
import java.util.concurrent.Executors
import java.io.File
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.async.Async.{async, await}

class Peekable extends Module {
  val a = IO(Output(UInt(32.W)))
  val c = Counter(128)
  c.inc()
  a := c.value
}

@State(Scope.Thread)
class BenchmarkState extends TestEnvInterface with HasTestName with Formal {
  final val ITER: Int = 20000

  var tester: BackendInstance[Peekable] = null
  var testname: String = null
  var topFileName: Option[String] = null

  @Setup(Level.Trial)
  def setup() {
    val testname = s"chisel_test_${System.currentTimeMillis()}"
    topFileName = Some(testname)
    batchedFailures.clear()
    val annotation = TargetDirAnnotation("test_run_dir" + File.separator + testname)
    tester = defaults.createDefaultTester(() => new Peekable(), Seq(annotation))
  }

  def getTestName = testname

  def test(testFn: Peekable => Unit) {
    Context.run(tester, this, testFn)
  }
}

class ChiselVerifyBenchmark {
  @Benchmark
  @JMHFork(value = 1, warmups = 1)
  def testbenchAsync(state: BenchmarkState) {
    implicit val ec = scala.concurrent.ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
    state.test { c =>
      c.clock.setTimeout(state.ITER*10)
      for (i <- 0 to state.ITER) {
        val fut = async {
          await(async { assert(c.a.peek().litValue == i % 128) })
          await(async { c.clock.step(1) })
        }
        Await.result(fut, 1.seconds)
      }
    }
  }

  @Benchmark
  @JMHFork(value = 1, warmups = 1)
  def testbenchAsyncRecursive(state: BenchmarkState) {
    implicit val ec = scala.concurrent.ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
    def f(i: Int, c: Peekable)(implicit ec: ExecutionContext): Future[Int] = if (i < state.ITER) async {
      await(async { assert(c.a.peek().litValue == i % 128) })
      await(async { c.clock.step(1) })
      await(f(i+1, c))
    } else async(i)
   state.test { c =>
      c.clock.setTimeout(state.ITER*10)
      Await.result(f(0, c)(ec), 1000.seconds)
    }
  }

  @Benchmark
  @JMHFork(value = 1, warmups = 1)
  def testbenchStraightLineChiselTest(state: BenchmarkState) {
    implicit val ec = scala.concurrent.ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
    state.test { c =>
      c.clock.setTimeout(state.ITER*10)
      for (i <- 0 to state.ITER) {
        assert(c.a.peek().litValue == i % 128)
        c.clock.step(1)
      }
    }
  }

  @Benchmark
  @JMHFork(value = 1, warmups = 1)
  def testbenchExpr(state: BenchmarkState) {
    implicit val ec = scala.concurrent.ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
    state.test { c =>
      c.clock.setTimeout(state.ITER*10)
      val program = repeat(for {
        y <- peek(c.a)
        _ <- step(1)
      } yield y, state.ITER)
      val res = VM.run(program, c.clock)
    }
  }
}
