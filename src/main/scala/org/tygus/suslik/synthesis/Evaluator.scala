package org.tygus.suslik.synthesis
import org.tygus.suslik.language.Expressions._
import org.tygus.suslik.language.Statements._
import org.tygus.suslik.logic.{Heaplet, PointsTo, SFormula}
import org.tygus.suslik.logic.Specifications.Assertion

import scala.collection.mutable.ListBuffer
object Evaluator {
  case class EvalResult(result: Assertion,
                        store: Map[Var, Expr]
                       )

  type Heap = Map[Int, Expr]
  type HeapE = Map[Expr, Expr]
  type Examples = List[(Subst, Heap, Heap)]
  // we let memory chunk size = 1 for now... this depends on unit concerned
  // typically addresses are stored in chunks of 8bits/1byte/multiples of 8
  val MEMORY_CHUNK_SIZE = 1
  def retrieve(v: Var, store:Subst) : Int = {
    v.subst(store) match {
      case HeapConst(x) => x
      case _ => throw new Exception("error")
    }
  }
  def resolveHeap(heap: Heap, store:Subst): Heap = {
    var  newHeap: Heap = Map()
    heap.foreach( item =>
      {
        val (k,v) = item
        newHeap += (k -> v.subst(store))
      }
    )
    newHeap
  }
  def resolveHeapLHS(heap:Heap, store:Subst):HeapE = {
    var  newHeap: HeapE = Map()
    heap.foreach( item =>
    {
      val (k,v) = item
      for (e <- store){
        val (c1,c2) = e
        c2 match {
          case IntConst(i) =>
            if (i == k){
              newHeap += (c1 -> v)
            }
          case HeapConst(i) =>
            if (i == k){
              newHeap += (c1 -> v)
            }
          case _ =>  throw new Exception("not supposed to happen")
        }
      }
    }
    )
    newHeap
  }
  def exampleResolvedHeapToSFormula(eg:HeapE):SFormula = {
    var sf = ListBuffer[Heaplet]()
    eg.foreach(item => {
      val (k,v) = item
      val newPointsTo = PointsTo(k, 0, v)
      sf += newPointsTo
    }
    )
    SFormula(sf.toList)
  }
  def evaluate(s: Statement, heap: Heap, store: Subst): (Heap, Subst)= {
    s match {
      case Skip => (heap, store)
      case SeqComp(s1,s2) => evaluate(s2, evaluate(s1,heap,store)._1, evaluate(s1,heap,store)._2)
      case Load(to, tpe, from, offset) => {
        val from_as_address = from.subst(store)
        val value_at_from = from_as_address match {
          case HeapConst(x) => heap.get(x)
          case _ => throw new Exception("not supposed to happen")
        }
        to.subst(store) match {
          case HeapConst(x) =>
            val y = x+offset
            value_at_from match {
              case Some(d) => (heap + (y -> d), store)
              case None => throw new Exception("not supposed to happen")
            }
          case IntConst(a) =>
            value_at_from match {
              case Some(x) =>
                (heap + (a+offset -> x), store )
              case None => throw new Exception("not supposed to happen")
            }
          case Var(a) =>
            value_at_from match {
              case Some(x) =>
                (heap, store + (Var(a) -> x ))
              case None => throw new Exception("not supposed to happen")
            }
        }
      }
      case Store(to, offset, e) => {
        val to_as_address = to.subst(store) match {
          case HeapConst(x) => x+offset
          case _ => ???
        }
        (heap + (to_as_address.asInstanceOf[Int] -> e),store)
      }
      case Free(v) =>
        val v_as_address = retrieve(v, store)
        (heap - v_as_address, store)
      case Malloc(to, tpe, sz) => {
        val to_as_address = retrieve(to, store)
        var new_heap = heap
        //using definition in Expressions that IntConst are null if they have value 0
        for(i <- 0 until sz){
          new_heap = new_heap + (i+to_as_address -> HeapConst(0) )
        }
        (new_heap, store)
      }
      case If(cond, tb, eb) => {
        val b =
          cond match {
          case BinaryExpr(op, left, right) =>
            val l = left.subst(store)
            val r = right.subst(store)
            val bool_res: Boolean = op match {
              case OpEq => l == r
              case OpLt => l < r
              case OpLeq => l <= r
              case _ => throw new Exception("not supposed to happen")
            }
            bool_res
          case _ => throw new Exception("Not supposed to happen")
        }
        if (b) {
          evaluate(tb, heap, store)
        } else {
          evaluate(eb, heap, store)
        }
      }
      case Guarded(cond, body, els, branchPoint) => {
        ???
      }
      case Call(fun, args, companion) => {
        ???
      }
      case Hole =>
        (Map.empty, Map.empty)
      case Error =>
        (Map.empty, Map.empty)
    }
  }

}