package u05lab.ex3

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils:
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]]:
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] =
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (msg.nonEmpty) println(msg + " -- \t" + duration.toNanos + " nanos;\t" + duration.toMillis + "ms")
    MeasurementResults(res, duration)

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

@main def checkPerformance: Unit =

  import PerformanceUtils.*
  val NUMELEMS = 1000000 //careful! numbers higher than 1000000 may take more than 1min and use many GB of RAM.

  /* Linear sequences: List, ListBuffer */
  println("/* Linear sequences: List, ListBuffer */")
  //CREATE
  measure("seq linear List       CREATE")({var lst = (1 to NUMELEMS).toList})
  var lst = (1 to NUMELEMS).toList
  import scala.collection.mutable.ListBuffer
  measure("seq linear ListBuffer CREATE")({val lstBuff = ListBuffer.from(1 to NUMELEMS)})
  val lstBuff = ListBuffer.from(1 to NUMELEMS)
  //READ
  measure("seq linear List       READ")({lst(200)})
  measure("seq linear ListBuffer READ")({lstBuff(200)})
  measure("seq linear List       READ")({lst.last})
  measure("seq linear ListBuffer READ")({lstBuff.last})
  measure("seq linear List       READ")({lst.size})
  measure("seq linear ListBuffer READ")({lstBuff.size})
  //UPDATE
  measure("seq linear List   HEAD UPDATE")({lst = 123::lst})
  measure("seq linear List   TAIL UPDATE")({lst = lst:+123})
  measure("seq linear ListBuffer  UPDATE")({lstBuff += 123})
  //DELETE
  measure("seq linear List        DELETE")({lst=lst.filterNot(_==123)})
  measure("seq linear ListBuffer  DELETE")({lstBuff -= 123})

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  println("/* Indexed sequences: Vector, Array, ArrayBuffer */")
  //CREATE
  measure("seq indexed Vector       CREATE")({var vec = (1 to NUMELEMS).toVector})
  var vec = (1 to NUMELEMS).toVector
  measure("seq indexed Array        CREATE")({var vec = (1 to NUMELEMS).toVector})
  val arr = (1 to NUMELEMS).toArray
  import scala.collection.mutable.ArrayBuffer
  measure("seq indexed Array Buffer CREATE")({var arrBuff = ArrayBuffer.from(1 to NUMELEMS)})
  val arrBuff = ArrayBuffer.from(1 to NUMELEMS)
  //READ
  measure("seq indexed Vector       READ")({vec(200)})
  measure("seq indexed Array        READ")({arr(200)})
  measure("seq indexed ArrayBuffer  READ")({arrBuff(200)})
  measure("seq indexed Vector       READ")({vec.last})
  measure("seq indexed Array        READ")({arr.last})
  measure("seq indexed ArrayBuffer  READ")({arrBuff.last})
  measure("seq indexed Vector       READ")({vec.size})
  measure("seq indexed Array        READ")({arr.size})
  measure("seq indexed ArrayBuffer  READ")({arrBuff.size})
  //UPDATE
  measure("seq indexed Vector       UPDATE")({vec = vec:+123})
  measure("seq indexed Array        UPDATE")({arr(3) = 123}) //array mutabile solo negli elementi
  measure("seq indexed ArrayBuffer  UPDATE")({arrBuff(3) = 123})
  measure("seq indexed ArrayBuffer  UPDATE")({arrBuff += 123})
  //DELETE
  measure("seq indexed Vector       DELETE")({vec = vec.filterNot(_==123)})
  measure("seq indexed Array        DELETE")({arr(3) -= 123}) //array mutabile solo negli elementi
  measure("seq indexed ArrayBuffer  DELETE")({arrBuff -= 123})


  /* Sets */
  println("/* Sets */")
  //CREATE
  measure("sets Set       CREATE")({var set = (1 to NUMELEMS).toSet})
  var set = (1 to NUMELEMS).toSet
  import scala.collection.mutable.{Set=>SetMut}
  measure("sets SetMut    CREATE")({var setMut = SetMut.from(1 to NUMELEMS)})
  val setMut = SetMut.from(1 to NUMELEMS)
  //READ
  measure("sets Set        READ")({set(200)})
  measure("sets SetMut     READ")({setMut(200)})
  measure("sets Set        READ")({set.last})
  measure("sets SetMut     READ")({setMut.last})
  measure("sets Set        READ")({set.size})
  measure("sets SetMut     READ")({setMut.size})
  //UPDATE
  measure("sets Set      UPDATE")({set = set+123})
  measure("sets SetMut   UPDATE")({setMut += 123})
  //DELETE
  measure("sets Set      DELETE")({set = set-123})
  measure("sets SetMut   DELETE")({setMut -= 123})


  /* Maps */
  println("/* Maps */")
  //CREATE
  measure("maps Map       CREATE")({var map = (1 to NUMELEMS).map((_,"str")).toMap})
  var map = (1 to NUMELEMS).map((_,"str")).toMap
  import scala.collection.mutable.{Map=>MapMut}
  measure("maps MapMut    CREATE")({var mapMut = MapMut.from((1 to NUMELEMS).map((_,"str")))})
  val mapMut = MapMut.from((1 to NUMELEMS).map((_,"str")))
  //READ
  measure("maps Map        READ")({map(200)})
  measure("maps MapMut     READ")({mapMut(200)})
  measure("maps Map        READ")({map.last})
  measure("maps MapMut     READ")({mapMut.last})
  measure("maps Map        READ")({map.size})
  measure("maps MapMut     READ")({mapMut.size})
  //UPDATE
  measure("maps Map      UPDATE")({map = map+((123,"str"))})
  measure("maps MapMut   UPDATE")({mapMut +=(123->"str")})
  //DELETE
  measure("maps Map      DELETE")({map = map-123})
  measure("maps MapMut   DELETE")({mapMut -= 123})
  
