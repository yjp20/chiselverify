package chiselverify

import chisel3._
import chiseltest._

import scala.collection.mutable.Set
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer

object VM {
  def run[R](expr: Expr[R], clock: chisel3.Clock): R = {
    val vm = new VM[R](clock)
    vm.run(expr)
  }
}

class VM[R](clock: chisel3.Clock) {
  private val running: Set[Thread[Any]] = new HashSet()
  private var threadCounter = 0
  private var time = 0

  def run(expr: Expr[R]): R = {
    val main = new Thread(expr)
    while (!running.isEmpty) {
      // println("TIME " + time)
      running.foreach {thread => {
        // println("  THREAD "  + thread.id)
        thread.continue() match {
          case Some(v) => {
            if (main == thread) return v.asInstanceOf[R]
            thread.done = true
          }
          case None => ()
        }
      }}
      running.toArray.foreach { thread => if (thread.done) {
        running -= thread
      }}

      clock.step(1)
      time += 1
    }
    ???
  }

  abstract class Monitor {}
  case class ThreadMonitor(ids: Seq[Thread[Any]]) extends Monitor
  case class ClockMonitor(time: Int) extends Monitor

  class Frame(val parent: Option[Frame], var expr: Expr[Any])

  protected class Apply[R1,R2](val cn: R1 => Expr[R2]) extends Expr[R2]
  protected class ApplyRepeat[R](val expr: Expr[R], val n: Int, var i: Int) extends Expr[R] {
    val returns = new ArrayBuffer[R](n)
  }
  protected class ApplyConcat[R](val exprs: Seq[Expr[R]], var i: Int) extends Expr[R] {
    val returns = new ArrayBuffer[R](exprs.length)
  }

  class Thread[+R](start: Expr[R]) extends Expr.Thread {
    val id = threadCounter
    var frame = new Frame(None, start)
    var monitor: Option[Monitor] = None
    var done = false

    threadCounter += 1
    running.add(this)

    // Continue execution of the thread until it encounters a yield point or
    // completes execution. Returns a None if not yet complete, returns Some(x)
    // if the thread has completed execution.
    def continue(): Option[R] = {
      val resolved = monitor match {
        case Some(monitor) => monitor match {
          case ThreadMonitor(threads) => threads.forall {thread => thread.done}
          case ClockMonitor(time) => time <= VM.this.time
        }
        case None => true
      }
      if (resolved) monitor = None

      while (monitor == None) {
        // `finished` will be some value if the main frame has finished
        // evaluation. If it hasn't finished yet, then it will return a None.
        val finished = frame.expr match {
          // Control
          case Cont(expr, cn) => {
            frame.expr = new Apply(cn)
            frame = new Frame(Some(frame), expr)
          }
          case x: Apply[_, _] => throw new RuntimeException();

          case Until(signal, expr) => if (signal.peek().litToBoolean == false) {
            frame = new Frame(Some(frame), expr)
          } else ret(())

          case Repeat(expr, n) => {
            frame.expr = new ApplyRepeat(expr, n, 0)
          }
          case x: ApplyRepeat[_] => if (x.i < x.n) {
            x.i = x.i + 1
            frame = new Frame(Some(frame), x.expr)
          } else ret(x.returns)

          case Concat(exprs) => if (!exprs.isEmpty) {
            frame.expr = new ApplyConcat(exprs,  0)
          }
          case x: ApplyConcat[_] => if (x.i < x.exprs.length) {
            frame = new Frame(Some(frame), x.exprs(x.i))
            x.i = x.i + 1
          } else ret(x.returns)

          case Join(threads) => {
            monitor = Some(new ThreadMonitor(threads.asInstanceOf[Seq[Thread[Any]]]))
            ret(())
          }

          case Fork(expr) => {
            ret(new Thread(expr).asInstanceOf[R])
          }

          case Step(cycles) => {
            monitor = Some(new ClockMonitor(VM.this.time + cycles))
            ret(())
          }

          // Primitives
          case Poke(signal, value) => ret(signal.poke(value))
          case Peek(signal) => ret(signal.peek())
          case Value(r) => ret(r)
          case Expect(signal, value) => {
            // println(signal.peek(), value)
            signal.expect(value)
            ret(())
          }
          case Clock() => ret(time)
          case Kill(thread) => {
            // println(thread.getClass())
            thread.asInstanceOf[Thread[Any]].done = true
            ret(())
          }
          case Debug(msg) => {
            // println(msg)
            ret(())
          }
        }
        finished match {
          case Some(v) => return Some(v.asInstanceOf[R])
          case _ => ()
        }
      }
      None
    }

    private def ret(v: Any): Option[R]  = {
      frame.parent match {
        case Some(p) => frame = p
        case None    => return Some(v.asInstanceOf[R])
      }
      frame.expr match {
        case x: Apply[Any, _] => frame.expr = x.cn(v)
        case _ => ()
      }
      None
    }
  }
}


