package chiselverify

import Expr._
import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import Expr.fork

class PrimitivesSpec extends AnyFlatSpec with ChiselScalatestTester {
  "peek" should "inspect the circuit IO and get its value" in {
    class Peekable extends Module {
      val a = IO(Output(UInt(32.W)))
      val c = Counter(16)
      c.inc() // free running counter
      a := c.value
    }

    test(new Peekable()) { c =>
      val program = for {
        v1 <- peek(c.a)
        _ <- step(10)
        v2 <- peek(c.a)
        _ <- step(6)
        v3 <- peek(c.a)
        _ <- step(1)
        v4 <- peek(c.a)
      } yield (v1.litValue, v2.litValue, v3.litValue, v4.litValue)
      val result = VM.run(program, c.clock)
      assert(result == (0, 10, 0, 1))
    }
  }

  "poke" should "drive the circuit IO" in {
    class Pokable extends Module {
      val a = IO(Input(UInt(32.W)))
      val aOut = IO(Output(UInt(32.W)))
      val aReg = RegNext(a) // pipelined loopback
      aOut := aReg
    }

    def pokeOne(signal: UInt, value: UInt): Expr[Unit] =
      for {
        _ <- poke(signal, value)
        _ <- step(1)
      } yield ()

    test(new Pokable()) { c =>
      val program = for {
        _ <- pokeOne(c.a, 100.U)
        v1 <- peek(c.aOut)
        _ <- step(100)
        v2 <- peek(c.aOut)
        _ <- pokeOne(c.a, 200.U)
        v3 <- peek(c.aOut)
      } yield (v1.litValue, v2.litValue, v3.litValue)
      val result = VM.run(program, c.clock)
      assert(result == (100, 100, 200))
    }
  }

  "fork/join" should "fork tasks at same time" in {
    class Pokable extends Module {
      val a = IO(Input(UInt(32.W)))
      val aOut = IO(Output(UInt(32.W)))
      val aReg = RegNext(a) // pipelined loopback
      aOut := aReg
    }

    test(new Pokable()) { c =>
      val program = for {
        ids <- repeat(fork(clock()), 10)
        times <- join(ids)
      } yield (ids)

      val result = VM.run(program, c.clock)
    }
  }

  "fork/join" should "work for queue example" in {
    class QueueModule[T <: Data](ioType: T, entries: Int) extends MultiIOModule {
      val in = IO(Flipped(Decoupled(ioType)))
      val out = IO(Decoupled(ioType))
      out <> Queue(in, entries)
    }

    test(new QueueModule(UInt(9.W), entries = 200)) { c =>
      c.in.initSource()
      c.in.setSourceClock(c.clock)
      c.out.initSink()
      c.out.setSinkClock(c.clock)
      val testVector = Seq.tabulate(300){ i => i.U }

      val program = for {
        a <- fork(enqueueSeq(c.in, testVector))
        b <- fork(dequeueSeq(c.out, testVector))
        _ <- join(Seq(a, b))
      } yield ()
      val result = VM.run(program, c.clock)
    }
  }
}
