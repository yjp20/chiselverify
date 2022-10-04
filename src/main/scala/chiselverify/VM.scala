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
    new Thread(expr)
    while (!threads.isEmpty) {
      threads.foreach {case (id, t) => {
        t.continue() match {
          case Some(v) => {
            if (id == 0) return v.asInstanceOf[R]
            threads.remove(id)
          }
          case None => {}
        }
      }}
      clock.step(1)
      time += 1
    }
    ???
  }

  abstract class Monitor {}
  case class ThreadMonitor(ids: Seq[Expr.ThreadID]) extends Monitor
  case class ClockMonitor(time: Int) extends Monitor

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
        frame.expr match {
          case Cont(expr, cn) => expr match {
            case Cont(_,_) => frame = new Frame(Some(frame), expr)
            case _ =>         frame.expr = cn(flat(expr))
          }
          case expr => {
            frame.parent match {
              case Some(p) => frame = p
              case None    => return Some(flat(expr.asInstanceOf[Expr[R]]))
            }
            frame.expr match {
              case Cont(_, cn) => frame.expr = cn(flat(expr))
              case _ => ???
            }
          }
        }
      }
      None
    }

    private def flat[R](expr: Expr[R]): R = {
      expr match {
        case Join(threadids) => monitor = Some(new ThreadMonitor(threadids))
        case Step(cycles) => monitor = Some(new ClockMonitor(VM.this.time + cycles))
        case Fork(expr) => new Thread(expr).id.asInstanceOf[R]
        case Poke(signal, value) => signal.poke(value)
        case Peek(signal) => signal.peek()
        case Value(r) => r
        case _ => ???
      }
    }
  }
}
