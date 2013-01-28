// author: Karel Čížek (kaja47@k47.cz) funkcionalne.cz
// license: new BSD

import sun.misc.Unsafe
import java.lang.reflect.{ Modifier, Field }
import scala.collection.{ mutable, immutable }

object SizeOf {
  val unsafe =
    acc(classOf[Unsafe].getDeclaredField("theUnsafe")).get().asInstanceOf[Unsafe]

  val NrBits  = System.getProperty("sun.arch.data.model").toInt
  val Word    = NrBits/8
  //val MinSize = 2*Word // 64bit + compressed oops -> 12B; 64bit -> 16B
  val MinSize = 16

  def acc(f: Field) = { f.setAccessible(true); f }

  def getFields(cl: Class[_]) =
    cl.getDeclaredFields.filter { f => (f.getModifiers & Modifier.STATIC) == 0 }

  def getAllFields(cl: Class[_]) =
    inheritanceChain(cl) flatMap getFields toIndexedSeq

  def inheritanceChain(cl: Class[_]) =
    Iterator.iterate[Class[_]](cl)(_.getSuperclass) takeWhile (_ != null)

  def sizeOf(x: AnyRef) = new Run().sizeOfRecursive(x)
  def sizeOfStats(x: AnyRef) = new Run().sizeOfStats(x)

  class Run {
    private val classSize    = collection.mutable.Map[Class[_], Int]()
    private val arraySumSize = collection.mutable.Map[Class[_], Int]() withDefaultValue 0
    private val classes      = collection.mutable.Map[Class[_], Int]() withDefaultValue 0 // counts
    private val visited      = collection.mutable.Set[Int]()

    private def isVisited(x: AnyRef) = visited(System.identityHashCode(x))

    private def sizeOfClass(cl: Class[_]) =
      classSize getOrElseUpdate (cl, {
        // Get the field with the maximum offset
        val offsets = getAllFields(cl) map unsafe.objectFieldOffset
        val maxOffset = if (offsets.nonEmpty) offsets.max else 16
        ((maxOffset.toInt / Word) + 1) * Word
      })

    private def sizeOfArray(x: Array[_]): Int = {
      val cl = x.getClass
      val len = unsafe.arrayBaseOffset(cl) + unsafe.arrayIndexScale(cl) * x.length
      val size = (((len-1) / Word) + 1) * Word

      visited += System.identityHashCode(x)
      classes(cl) = classes(cl) + 1
      arraySumSize(cl) = arraySumSize(cl) + size

      size
    }

    private def sizeOfObj(x: AnyRef): Int = {
      val cl = x.getClass

      visited += System.identityHashCode(x)
      classes(cl) = classes(cl) + 1

      sizeOfClass(cl)
    }

    def sizeOfRecursive(x: AnyRef): Int = x match {
      case null => 0
      case arr: Array[AnyRef] =>
        sizeOfArray(arr) + (arr filterNot isVisited map sizeOfRecursive sum)
      case arr: Array[_] =>
        sizeOfArray(arr)
      case obj =>
        val fs = getAllFields(obj.getClass) filterNot (_.getType.isPrimitive)
        sizeOfObj(obj) + (fs map { f => acc(f) get obj } filterNot isVisited map sizeOfRecursive sum)
    }

    def sizeOfStats(x: AnyRef): Unit = {
      val size = sizeOfRecursive(x)

      println()
      println("\ntotal "+size+"\n")
      println()

      println()
      classSize foreach { case (cl, size) =>
        println(cl.getName)
        getAllFields(cl) sortBy unsafe.objectFieldOffset foreach { f =>
          println("%2d %s.%s: %s" format (unsafe.objectFieldOffset(f), f.getDeclaringClass.getName, f.getName, f.getType.getName))
        }
        println(size+"\n")
      }

      println("  count      one         sum")
      println("----------------------------")
      val cc = classes map { case (cl, count) =>
        val one   = classSize get cl                 getOrElse (arraySumSize(cl) / count)
        val total = classSize get cl map (_ * count) getOrElse (arraySumSize(cl))
        (cl, count, one, total)
      }
      cc foreach { case (cl, count, one, total) =>
        printf("%7d %7dB %10dB %s\n", count, one, total, cl.getName)
      }

      val count   =  cc map (_._2) sum
      val avgSize = (cc map (_._3) sum) / cc.size
      val totSize =  cc map (_._4) sum

      println("----------------------------")
      printf("%7d %7dB %10dB\n", count, avgSize, totSize)
    }
  }
}

//val x = Vector(1 to 1025: _*)
//val tupleSeq: Seq[(Int, Int)] = (1 to 100 map (i => i -> i)) :+ (1,1)
//val x = Map(tupleSeq:_*v)
//val x = collection.immutable.SortedSet(1 to 200: _*)
//val x = List(1 to 200: _*)
val x = immutable.HashSet(1 to 10000: _*)
//val x = new java.util.ArrayList[Int]; for (i <- 0 to 100000) x.add(i)

SizeOf.sizeOfStats(x)
