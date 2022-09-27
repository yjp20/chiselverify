package chiselverify

import chisel3._
import chiseltest._

class VM(clock: Clock) {
  def run[R](cmd: Expr[R]): R = {
    cmd match {
      case Cont(r, fn) => run(fn(flatRun(r)))
      case _: Expr[_] => flatRun(cmd)
    }
  }

  private def flatRun[R](cmd: Expr[R]): R = {
    cmd match {
      case Value(r) => r
      case Poke(signal, value) => signal.poke(value)
      case Peek(signal) => signal.peek()
      case Step(cycles) => this.clock.step(cycles)
    }
  }
}
