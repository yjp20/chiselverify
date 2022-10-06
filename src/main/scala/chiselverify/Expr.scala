package chiselverify

import chisel3._
import chisel3.util._
import chiseltest._

object Expr {
  type ThreadID = Int

  // Primitives
  def poke[R <: Data](signal: R, value: R): Expr[Unit] = Poke(signal, value)
  def peek[R <: Data](signal: R): Expr[R] = Peek(signal)
  def expect[R <: Data](signal: R, value: R): Expr[Unit] = Expect(signal, value)
  def debug(msg: String): Expr[Unit] = Debug(msg)

  // Control
  def step(cycles: Int): Expr[Unit] = Step(cycles)
  def fork[R](expr: Expr[R]): Expr[Expr.ThreadID] = Fork(expr)
  def join(threadid: Expr.ThreadID): Expr[Unit] = Join(Seq(threadid))
  def join(threadids: Seq[Expr.ThreadID]): Expr[Unit] = Join(threadids)
  def until[R](signal: Bool, expr: Expr[R]): Expr[Unit] = Until(signal, expr)

  // Combinators
  def repeat(expr: Expr[_], n: Int): Expr[Unit] = Repeat(expr, n)
  def concat(exprs: Seq[Expr[_]]): Expr[Unit] = Concat(exprs)

  // Utilities
  def enqueue[R <: Data](x: ReadyValidIO[R], data: R): Expr[Unit] = for {
    _ <- debug("TRY_ENQUEUE")
    _ <- poke(x.bits, data)
    _ <- poke(x.valid, true.B)
    t <- fork(for {
      _ <- until(x.ready, step(1))
      _ <- debug("DONE_ENQUEUE")
    } yield ())
    _ <- join(t)
    _ <- step(1)
  } yield ()
  def enqueueSeq[R <: Data](x: ReadyValidIO[R], data: Seq[R]): Expr[Unit] =
    concat(data.map { d => enqueue(x, d) })

  def dequeue[R <: Data](x: ReadyValidIO[R], data: R): Expr[Unit] = for {
    _ <- debug("TRY_DEQUEUE")
    _ <- poke(x.ready, true.B) 
    t <- fork(for {
      _ <- until(x.valid, step(1))
      _ <- expect(x.bits, data)
      _ <- debug("DONE_DEQUEUE")
    } yield ())
    _ <- join(t)
    _ <- step(1)
  } yield ()
  def dequeueSeq[R <: Data](x: ReadyValidIO[R], data: Seq[R]): Expr[Unit] =
    concat(data.map { d => dequeue(x, d) })

}

abstract class Expr[+R] {
  def map[R2](fn: R => R2): Expr[R2] = this.flatMap[R2](r => Value(fn(r)))
  def flatMap[R2](fn: R => Expr[R2]): Expr[R2] = this match {
    case Value(v) => fn(v)
    case _ => Cont(this, fn)
  }
}

// Intrinsics
protected case class Cont[R1, R2](expr: Expr[R1], cn: R1 => Expr[R2]) extends Expr[R2]
protected case class Value[R](value: R) extends Expr[R]
// Primitives
protected case class Poke[R <: Data](signal: R, value: R) extends Expr[Unit]
protected case class Peek[R <: Data](signal: R) extends Expr[R]
protected case class Expect[R <: Data](signal: R, value: R) extends Expr[Unit]
protected case class Debug(msg: String) extends Expr[Unit]
// Control
protected case class Step(cycles: Int) extends Expr[Unit]
protected case class Fork[R](expr: Expr[R]) extends Expr[Expr.ThreadID]
protected case class Join(threadids: Seq[Expr.ThreadID]) extends Expr[Unit]
protected case class Until[R](signal: Bool, expr: Expr[R]) extends Expr[Unit]
protected case class Repeat(expr: Expr[_], n: Int) extends Expr[Unit]
protected case class Concat(exprs: Seq[Expr[_]]) extends Expr[Unit]

