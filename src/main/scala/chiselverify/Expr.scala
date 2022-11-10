package chiselverify

import chisel3._
import chisel3.util._
import chiseltest._

object Expr {
  abstract class Thread

  // Primitives
  def poke[R <: Data](signal: R, value: R): Expr[Unit] = Poke(signal, value)
  def peek[R <: Data](signal: R): Expr[R] = Peek(signal)
  def expect[R <: Data](signal: R, value: R): Expr[Unit] = Expect(signal, value)
  def debug(msg: String): Expr[Unit] = Debug(msg)

  // Control
  def step(cycles: Int): Expr[Unit] = Step(cycles)
  def fork[R](expr: Expr[R]): Expr[Thread] = Fork(expr)
  def join(thread: Thread): Expr[Unit] = Join(Seq(thread))
  def join(threads: Seq[Thread]): Expr[Unit] = Join(threads)
  def clock() = Clock()
  def until[R](signal: Bool, expr: Expr[R]): Expr[Unit] = Until(signal, expr)
  def kill(thread: Thread): Expr[Bool] = Kill(thread)

  // Combinators
  def repeat[R](expr: Expr[R], n: Int): Expr[Seq[R]] = Repeat(expr, n)
  def concat[R](exprs: Seq[Expr[R]]): Expr[Seq[R]] = Concat(exprs.toVector)

  // Utilties
  def enqueue[R <: Data](x: DecoupledIO[R], data: R): Expr[Unit] = for {
    _ <- poke(x.bits, data)
    _ <- poke(x.valid, true.B)
    t <- fork(for {
      _ <- until(x.ready, step(1))
    } yield ())
    _ <- join(t)
    _ <- step(1)
  } yield ()
  def enqueueSeq[R <: Data](x: DecoupledIO[R], data: Seq[R]): Expr[Seq[Unit]] =
    concat(data.map { d => enqueue(x, d) })

  def dequeue[R <: Data](x: DecoupledIO[R], data: R): Expr[Unit] = for {
    _ <- poke(x.ready, true.B)
    t <- fork(for {
      _ <- until(x.valid, step(1))
      _ <- expect(x.bits, data)
    } yield ())
    _ <- join(t)
    _ <- step(1)
  } yield ()
  def dequeueSeq[R <: Data](x: DecoupledIO[R], data: Seq[R]): Expr[Seq[Unit]] =
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
protected case class Clock() extends Expr[Int]
// Control
protected case class Step(cycles: Int) extends Expr[Unit]
protected case class Fork[R](expr: Expr[R]) extends Expr[Expr.Thread]
protected case class Join(threads: Seq[Expr.Thread]) extends Expr[Unit]
protected case class Until[R](signal: Bool, expr: Expr[R]) extends Expr[Unit]
protected case class Repeat[R](expr: Expr[R], n: Int) extends Expr[Seq[R]]
protected case class Concat[R](exprs: Vector[Expr[R]]) extends Expr[Seq[R]]
protected case class Kill(thread: Expr.Thread) extends Expr[Bool]

