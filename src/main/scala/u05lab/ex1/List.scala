package u05lab.ex1

import u05lab.ex1.List

import scala.::

// Ex 1. implement the missing methods both with recursion or with using fold, map, flatMap, and filters
// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)
    case _ => None

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def append(list: List[A]): List[A] = this match
    case h :: t => h :: t.append(list)
    case _ => list

  def :+(elem:A): List[A] = this.append(List(elem))

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filter(predicate)
    case _ :: t => t.filter(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.map(fun)
    case _ => Nil()

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight[List[B]](Nil())(f(_) append _)

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(z, h))(op)
    case Nil() => z

  def foldRight[B](z: B)(f: (A, B) => B): B = this match
    case h :: t => f(h, t.foldRight(z)(f))
    case _ => z

  def length: Int = foldLeft(0)((l, _) => l + 1)

  def isEmpty: Boolean = this match
    case Nil() => true
    case _ => false

  def reverse(): List[A] = foldLeft[List[A]](Nil())((l, e) => e :: l)

  /** EXERCISES */

  def zipRight: List[(A, Int)] =
    var i= -1
    map(elem => {i+=1;(elem, i)})

  def zipRightRec: List[(A, Int)] =
    def _zipRight(l: List[A], n: Int): List[(A, Int)] = l match
      case h :: t => (h, n) :: _zipRight(t, n + 1)
      case _ => Nil()

    _zipRight(this, 0)

  def zipRight1: List[(A, Int)] =
    this.foldRight((Nil[(A, Int)](), this.length))((elem, acc) => ( (elem, acc._2 -1)::acc._1 , acc._2 - 1 ))._1
    //start right | accumulator: (built_list, int) | for new elem>(built_list is: (elem, int -1) concat to built_list),
    //                           ( acc._1 ,acc._2)                                             int is now : int - 1)
    //                                                                   foldRight returns the (built_list, int), we return only _1 (built list)


  def partition(pred: A => Boolean): (List[A], List[A]) =
    //start right | accumulator (List,List) | for new elem =>
    this.foldRight( (Nil[A](), Nil[A]()) )( (elem, accCouple) => elem match
      case elem if pred(elem) => (elem::accCouple._1, accCouple._2) //accumulator now is (elem added list1, list2)
      case _ => (accCouple._1, elem::accCouple._2) //accumulator now is (list1, elem added list2)
    )

  def partitionRec(pred: A => Boolean): (List[A], List[A]) = this match
    case h :: t if pred(h) => val coupleLists = t.partitionRec(pred); (h :: coupleLists._1, coupleLists._2)
    case h :: t =>            val coupleLists = t.partitionRec(pred); (coupleLists._1, h :: coupleLists._2)
    case _ => (Nil(), Nil())

  def partition2(pred: A => Boolean): (List[A], List[A]) = (filter(pred), filter(A => !pred(A)))
  //tuple with a list that is filtered with pred and a list that is filtered with the opposite of pred  /!\COSTSx2!

  def span(pred: A => Boolean): (List[A], List[A]) =
    //start left| accumulator ((List,List),valid) | for new elem =>
    this.foldLeft( ((Nil[A](), Nil[A]()), true) )( (accTuple, elem) => elem match
      case elem if pred(elem) && accTuple._2 => ((accTuple._1._1:+elem, accTuple._1._2), accTuple._2) //accumulator now is ((list1+elem, list2), valid)
      case _ => ((accTuple._1._1, accTuple._1._2:+elem), false) //accumulator now is ((list1, list2+elem), INVALID) //INVALID will now on invalidate the case above even if pred(elem) is true
    )._1 //since foldLeft returns ((List,List),valid) we take ._1 to get (List,List)
         // also that's why we didn't use (List, List, valid), cause then we had to make a tuple with two elements from a tuple with three


  def spanRec(pred: A => Boolean): (List[A], List[A]) = this match
    case h :: t if pred(h) => val coupleLists = t.spanRec(pred); (h :: coupleLists._1, coupleLists._2)
    case _ => (Nil(), this)

  /** @throws UnsupportedOperationException if the list is empty */
  def reduce(op: (A, A) => A): A = this match
    case h :: t => t.foldRight(h)(op) //fold left with head as initializer
    case Nil() => throw UnsupportedOperationException() //if list empty (no head) throws exception

  def takeRight(n: Int): List[A] = ???

// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

@main def checkBehaviour(): Unit =
  val reference = List(1, 2, 3, 4)
  println(reference.zipRight) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.reduce(_ + _)) // 10
  try Nil.reduce[Int](_ + _)
  catch case ex: Exception => println(ex) // prints exception
  println(List(10).reduce(_ + _)) // 10
  println(reference.takeRight(3)) // List(2, 3, 4)
