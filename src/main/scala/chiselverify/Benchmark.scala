package chiselverify

import chiselverify.Expr._

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.simulator.TreadleBackendAnnotation
import chiseltest.internal._

import org.openjdk.jmh.annotations.{Benchmark, Fork => JMHFork, Mode, BenchmarkMode}
import java.util.concurrent.Executors
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.async.Async.{async, await}

class ChiselVerifyBenchmark {
  @Benchmark
  @JMHFork(value = 1, warmups = 1)
  def testbenchAsync() {
    ChiselVerifyTestBench.testbenchAsync()
  }

  @Benchmark
  @JMHFork(value = 1, warmups = 1)
  def testbenchAsyncRecursive() {
    ChiselVerifyTestBench.testbenchAsyncRecursive()
  }

  @Benchmark
  @JMHFork(value = 1, warmups = 1)
  def testbenchStraightLineChiselTest() {
    ChiselVerifyTestBench.testbenchStraightLineChiselTest()
  }

  @Benchmark
  @JMHFork(value = 1, warmups = 1)
  def testbenchExpr() {
    ChiselVerifyTestBench.testbenchExpr()
  }
}


class Peekable extends Module {
  val a = IO(Output(UInt(32.W)))
  val c = Counter(128)
  c.inc()
  a := c.value
}

object ChiselVerifyTestBench {
  final val ITER: Int = 20000

  def testbenchAsync() {
    implicit val ec = scala.concurrent.ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

    RawTester.test(new Peekable()) { c =>
      c.clock.setTimeout(ITER*10)
      for (i <- 0 to ITER) {
        val fut = async {
          await(async { assert(c.a.peek().litValue == i % 128) })
          await(async { c.clock.step(1) })
        }
        Await.result(fut, 1.seconds)
      }
    }
  }

  def testbenchAsyncRecursive() {
    implicit val ec = scala.concurrent.ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

    def f(i: Int, c: Peekable)(implicit ec: ExecutionContext): Future[Int] = if (i < ITER) async {
      await(async { assert(c.a.peek().litValue == i % 128) })
      await(async { c.clock.step(1) })
      await(f(i+1, c))
    } else async(i)

    RawTester.test(new Peekable()) { c =>
      c.clock.setTimeout(ITER*10)
      Await.result(f(0, c)(ec), 1000.seconds)
    }
  }

  def testbenchStraightLineChiselTest() {
    RawTester.test(new Peekable()) { c =>
      c.clock.setTimeout(ITER*10)
      for (i <- 0 to ITER) {
        assert(c.a.peek().litValue == i % 128)
        c.clock.step(1)
      }
    }
  }

  def testbenchExpr() {
    RawTester.test(new Peekable()) { c =>
      c.clock.setTimeout(ITER*10)
      val program = repeat(for {
        y <- peek(c.a)
        _ <- step(1)
      } yield y, ITER)
      val res = VM.run(program, c.clock)
    }
  }
}
