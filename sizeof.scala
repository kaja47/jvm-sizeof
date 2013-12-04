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
    private val classSize    = mutable.Map[Class[_], Int]()
    private val arraySumSize = mutable.Map[Class[_], Int]() withDefaultValue 0
    private val arrayLengths = mutable.Map[Class[_], mutable.ArrayBuilder.ofInt]()
    private val classes      = mutable.Map[Class[_], Int]() withDefaultValue 0 // counts
    private val visited      = new java.util.IdentityHashMap[AnyRef, Unit]

    private def isVisited(x: AnyRef) = visited.containsKey(x)

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

      visited.put(x, ())
      classes(cl) = classes(cl) + 1
      arraySumSize(cl) += size
      (arrayLengths getOrElseUpdate (cl, new mutable.ArrayBuilder.ofInt)) += x.length

      size
    }

    private def sizeOfObj(x: AnyRef): Int = {
      val cl = x.getClass

      visited.put(x, ())
      classes(cl) = classes(cl) + 1

      sizeOfClass(cl)
    }

    def sizeOfRecursive(x: AnyRef): Int =
      sizeOfTailRecursive(Array(x), 0)

    private def _sizeOfTailRecursive(x: AnyRef): (Seq[AnyRef], Int) = x match {
      case null =>
        (Seq(), 0)
      case arr: Array[AnyRef] =>
        (arr filterNot isVisited, sizeOfArray(arr))
      case arr: Array[_] =>
        (Seq(), sizeOfArray(arr))
      case obj =>
        val fs = getAllFields(obj.getClass) filterNot (_.getType.isPrimitive)
        (fs map { f => acc(f) get obj } filterNot isVisited, sizeOfObj(obj))
    }

    @annotation.tailrec
    final def sizeOfTailRecursive(xs: Array[AnyRef], size: Int): Int = {
      if (xs.isEmpty) {
        size
      } else {
        val rs = xs map _sizeOfTailRecursive
        val newSize = rs map (_._2) sum ;
        val children = rs flatMap (_._1)
        sizeOfTailRecursive(children, newSize + size)
      }
    }

    def sizeOfStats(x: AnyRef): Unit = {
      val size = sizeOfRecursive(x)
      def freq(xs: Array[Int]): Seq[(Int, Int)] =
        xs.groupBy(identity).mapValues(_.size).toSeq.sortBy(_._1)

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
        (cl, count, one, total, arrayLengths get cl)
      }
      cc foreach { case (cl, count, one, total, arrSize) =>
        val arrStat = arrSize map (as => freq(as.result).map { case (s, c) => s+" - "+c+"x" }.mkString("sizes: (", ", ", ")")) getOrElse ""
        printf("%7d %7dB %10dB %s %s\n", count, one, total, cl.getName, arrStat)
      }

      val count   =  cc map (_._2) sum
      val avgSize = (cc map (_._3) sum) / cc.size
      val totSize =  cc map (_._4) sum

      println("----------------------------")
      printf("%7d %7s  %10dB\n", count, "", totSize)

      println()
      println("total size: "+size+" bytes")
      val collSize = x match {
        case x: collection.GenTraversableOnce[_] => x.size
        case x: java.util.Collection[_] => x.size
        case _ => 0
      }
      if (collSize > 0)
        println("per element: "+(size.toDouble / collSize)+" bytes")

      println()
    }
  }
}

//val x = Vector(1 to 1025: _*)
//val tupleSeq: Seq[(Int, Int)] = (1 to 100 map (i => i -> i)) :+ (1,1)
//val x = Map(tupleSeq:_*v)
//val x = collection.immutable.SortedSet(1 to 200: _*)
//val x = List(1 to 200: _*)
//val x = immutable.HashSet(1 to 10000: _*)
//val x = new java.util.ArrayList[Int]; for (i <- 0 to 100000) x.add(i)

//SizeOf.sizeOfStats(x)
