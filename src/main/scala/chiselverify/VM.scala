package chiselverify

import chisel3._
import chiseltest._

import scala.collection.mutable.Set
import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue
import scala.collection.mutable.ArrayBuffer

object VM {
  def run[R](expr: Expr[R], clock: chisel3.Clock): R = {
    val vm = new VM[R](clock)
    vm.run(expr)
  }
}

class Frame(val parent: Option[Frame], var expr: Expr[Any])
private class Apply[R1,R2](val cn: R1 => Expr[R2]) extends Expr[R2]
private class ApplyRepeat[R](val expr: Expr[R], val n: Int, var i: Int) extends Expr[R] {
  val returns = new ArrayBuffer[R](n)
}
private class ApplyConcat[R](val exprs: Seq[Expr[R]], var i: Int) extends Expr[R] {
  val returns = new ArrayBuffer[R](exprs.length)
}

class VM[R](clock: chisel3.Clock) {
  private val alive = new HashSet[Thread]()
  private val queue = new Queue[Thread]()
  private var threadCounter = 0
  private var time = 0

  def run(expr: Expr[R]): R = {
    val main = new Thread(expr, Some("main"))
    while (alive.nonEmpty) {
      if (time < 20) println("== " + time)
      queue ++= alive
      while (!queue.isEmpty) {
        val thread = queue.dequeue()
        thread.returns = thread.continue()
        if (time < 20) thread.typename match {
          case Some(x) => println(" %d %s".format(thread.id, x))
          case None    => println(" %d".format(thread.id))
        }
        if (thread.returns.isDefined) {
          // Terminate thread now that it is done by removing it from the alive pool
          alive -= thread
          // Wakeup the waiting threads for this particular thread
          alive ++= thread.waiting
        }
      }
      clock.step(1)
      time += 1
    }
    main.returns.get.asInstanceOf[R]
  }


  abstract class Monitor{
    def isResolved(): Boolean
  }
  case class ThreadMonitor(threads: Seq[Thread]) extends Monitor {
    def isResolved = threads.forall {_.returns.isDefined}
  }
  case class TimeMonitor(time: Int) extends Monitor {
    def isResolved = time <= VM.this.time
  }


  class Thread(start: Expr[Any], name: Option[String]) extends Expr.Thread(name) {
    val id = threadCounter
    var frame = new Frame(None, start)
    var monitor: Option[Monitor] = None
    var returns: Option[Any] = None

    // A queue of threads waiting on the current thread to finish
    var waiting = new Queue[Thread]()

    threadCounter += 1
    alive += this
    queue += this

    // Continue execution of the thread until it encounters a yield point or
    // completes execution. Returns a None if not yet complete, returns Some(x)
    // if the thread has completed execution.
    def continue(): Option[Any] = {
      if (monitor.forall {_.isResolved}) monitor = None

      while (monitor == None && returns.isEmpty) {
        returns = frame.expr match {
          // Control
          case Cont(expr, cn) => {
            frame.expr = new Apply(cn)
            frame = new Frame(Some(frame), expr)
            None
          }
          case Until(signal, expr) => if (signal.peek().litToBoolean == false) {
            frame = new Frame(Some(frame), expr)
            None
          } else ret(())

          case Repeat(expr, n) => {
            frame.expr = new ApplyRepeat(expr, n, 0)
            None
          }
          case x: ApplyRepeat[_] => if (x.i < x.n) {
            x.i = x.i + 1
            frame = new Frame(Some(frame), x.expr)
            None
          } else ret(x.returns)

          case Concat(exprs) => {
            if (!exprs.isEmpty) frame.expr = new ApplyConcat(exprs,  0)
            None
          }
          case x: ApplyConcat[_] => if (x.i < x.exprs.length) {
            frame = new Frame(Some(frame), x.exprs(x.i))
            x.i = x.i + 1
            None
          } else ret(x.returns)

          case Join(threads) => {
            monitor = Some(new ThreadMonitor(threads.asInstanceOf[Seq[Thread]]))
            ret(())
          }

          case Fork(expr, typename) => {
            ret(new Thread(expr, typename))
          }

          case Step(cycles) => {
            monitor = Some(new TimeMonitor(VM.this.time + cycles))
            ret(())
          }

          // Primitives
          case Poke(signal, value) => ret(signal.poke(value))
          case Peek(signal) => ret(signal.peek())
          case Value(r) => ret(r)
          case Expect(signal, value) => {
            signal.expect(value)
            ret(())
          }
          case Time() => ret(time)
          case Kill(thread) => {
            thread.asInstanceOf[Thread].returns = Some(())
            ret(())
          }
          case Debug(msg) => {
            println(msg)
            ret(())
          }
        }
      }
      if (monitor != None) {

      }
      returns
    }

    private def ret(v: Any): Option[Any] = {
      // If there is a parent frame, then escape to that frame. If not, then
      // the root expression is fully evaluated sdo return a value
      frame.parent match {
        case Some(p) => frame = p
        case None    => return Some(v)
      }
      // Special case for Cont(x, cn) expressions (which leave behind Apply(cn)
      // as a resuidual, immediately run the continuation and resolve the next
      // Expr
      frame.expr match {
        case x: Apply[_, _] => frame.expr = x.cn.asInstanceOf[Any => Expr[R]](v)
        case _ => ()
      }
      None
    }
  }
}
