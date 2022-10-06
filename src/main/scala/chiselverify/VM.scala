package chiselverify

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import chisel3._
import chiseltest._

object VM {
  def run[R](expr: Expr[R], clock: Clock): R = {
    val vm = new VM[R](clock)
    vm.run(expr)
  }
}

protected class VM[R](clock: Clock) {
  private val threads: Map[Expr.ThreadID, Thread[Any]] = new HashMap()
  var threadCounter = 0
  var time = 0

  def run(expr: Expr[R]): R = {
    val main = new Thread(expr)
    while (!threads.isEmpty) {
      threads.foreach {case (id, thread) => {
        thread.continue() match {
          case Some(v) => {
            if (main == thread) return v.asInstanceOf[R]
            threads.remove(id)
          }
          case None => ()
        }
      }}
      clock.step(1)
      time += 1
      println("==== TIME")
    }
    ???
  }

  private class Thread[+R](start: Expr[R]) {
    var frame = new Frame(None, start)
    var monitor: Option[Monitor] = None
    val id: Expr.ThreadID = threadCounter

    threadCounter += 1
    threads(id) = this

    protected class Frame(val parent: Option[Frame], var expr: Expr[Any])

    def continue(): Option[R] = {
      val resolved = monitor match {
        case Some(monitor) => monitor match {
          case ThreadMonitor(ids) => ids.forall(id => !threads.contains(id))
          case ClockMonitor(time) => time <= VM.this.time
        }
        case None => true
      }
      if (resolved) monitor = None

      while (monitor == None) {
        val finished = frame.expr match {
          case Cont(expr, cn) => frame = new Frame(Some(frame), expr)
          case Until(signal, expr) => {
            if (signal.peek().litToBoolean == false) {
              frame = new Frame(Some(frame), expr)
            } else ret(())
          }
          case Repeat(expr, n) => if (n > 0) {
            frame.expr = Expr.repeat(expr, n-1)
            frame = new Frame(Some(frame), expr)
          } else ret(())
          case Concat(exprs) => if (!exprs.isEmpty) {
            val expr = exprs(0)
            frame.expr = Expr.concat(exprs.slice(1,exprs.length))
            frame = new Frame(Some(frame), expr)
          } else ret(())
          case Join(threadids) => {
            monitor = Some(new ThreadMonitor(threadids))
            ret(())
          }
          case Step(cycles) => {
            monitor = Some(new ClockMonitor(VM.this.time + cycles))
            ret(())
          }
          case Fork(expr) => ret(new Thread(expr).id.asInstanceOf[R])
          case Poke(signal, value) => ret(signal.poke(value))
          case Peek(signal) => ret(signal.peek())
          case Value(r) => ret(r)
          case Expect(signal, value) => {
            println(signal, value)
            signal.expect(value)
            ret(())
          }
          case Debug(msg) => {
            println(msg)
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
        case Cont(_, cn) => frame.expr = cn(v)
        case _ => ()
      }
      None
    }
  }
}

private abstract class Monitor {}
private case class ThreadMonitor(ids: Seq[Expr.ThreadID]) extends Monitor
private case class ClockMonitor(time: Int) extends Monitor

