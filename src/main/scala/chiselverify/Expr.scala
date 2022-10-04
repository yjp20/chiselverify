package chiselverify

import chisel3._
import chiseltest._

object Expr {
  type ThreadID = Int

  def poke[R <: Data](signal: R, value: R): Expr[Unit] = Poke(signal, value)
  def peek[R <: Data](signal: R): Expr[R] = Peek(signal)
  def step(cycles: Int): Expr[Unit] = Step(cycles)
}

abstract class Expr[+R] {
  def map[R2](fn: R => R2): Expr[R2] = this.flatMap[R2](r => Value(fn(r)))
  def flatMap[R2](fn: R => Expr[R2]): Expr[R2] = this match {
    case Value(v) => fn(v)
    case _ => Cont(this, fn)
  }
}

protected case class Cont[R1, R2](expr: Expr[R1], cn: R1 => Expr[R2]) extends Expr[R2]
protected case class Apply[R1, R2](cn: R1 => Expr[R2]) extends Expr[R2]
protected case class Poke[R <: Data](signal: R, value: R) extends Expr[Unit]
protected case class Peek[R <: Data](signal: R) extends Expr[R]
protected case class Step(cycles: Int) extends Expr[Unit]
protected case class Value[R](value: R) extends Expr[R]
protected case class Fork[R](expr: Expr[R]) extends Expr[R]
protected case class Join(threadids: Seq[Expr.ThreadID]) extends Expr[Unit]

