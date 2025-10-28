package flat

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object util:
  def bigProduct[T](xss: Seq[Seq[T]]): Seq[Seq[T]] = xss match
    case Nil => Seq.empty
    case Seq(xs) => for x <- xs yield Seq(x)
    case xs +: yss => for x <- xs; ys <- bigProduct(yss) yield x +: ys

  private val unicodeSubscripts: String = "₀₁₂₃₄₅₆₇₈₉"

  def renderSubscript(k: Int): String =
    require(k >= 0)
    k.toString.map(c => unicodeSubscripts(c - '0'))

  private val mxBean =
    java.lang.management.ManagementFactory.getPlatformMXBean(classOf[java.lang.management.ThreadMXBean])

  final class Stopwatch:
    private var startTime = 0L

    def start(): Unit =
      startTime = mxBean.getCurrentThreadCpuTime

    def stop(): Double =
      val endTime = mxBean.getCurrentThreadCpuTime
      (endTime - startTime) / 1.0e6

  enum Aggregator:
    case AllCount
    case AllTime

  import Aggregator.*

  final class MetricCollector(outputPath: os.Path, aggregators: Aggregator*):
    private class Block(val label: String):
      // Key-value pairs
      val counts = mutable.Map.empty[String, Int]
      val times = mutable.Map.empty[String, Long]
      val values = mutable.Map.empty[String, ujson.Value]
      // Nested blocks
      val blocks = mutable.Map.empty[String, ListBuffer[Block]]
      // Auxiliaries for computing elapsed time
      val startTimes = mutable.Map.empty[String, Long]
      val hasStarted = mutable.Map.empty[String, Boolean]

    private val st = mutable.Stack(Block(""))

    def push(label: String): Unit =
      require(!st.top.counts.keySet.contains(label))
      require(!st.top.times.keySet.contains(label))
      require(!st.top.values.keySet.contains(label))
      if !st.top.blocks.keySet.contains(label) then
        st.top.blocks(label) = ListBuffer.empty
      st.push(Block(label))

    def pop(): Unit =
      val block = st.pop()
      st.top.blocks(block.label) += block

    def put(key: String, value: ujson.Value): Unit =
      st.top.values(key) = value

    def count(key: String, n: Int = 1): Unit =
      val m = st.top.counts
      m(key) = m.getOrElse(key, 0) + n

    def timeStart(key: String): Unit =
      require(!st.top.hasStarted.getOrElse(key, false), s"stopwatch '$key' has started")
      st.top.hasStarted(key) = true
      st.top.startTimes(key) = mxBean.getCurrentThreadCpuTime

    def timePause(key: String): Unit =
      val endTime = mxBean.getCurrentThreadCpuTime
      require(st.top.hasStarted.getOrElse(key, false), s"stopwatch '$key' has not yet started")
      st.top.hasStarted(key) = false
      val m = st.top.times
      m(key) = m.getOrElse(key, 0L) + (endTime - st.top.startTimes(key))

    def save(indent: Int = -1, sortKeys: Boolean = false): Unit =
      require(st.size == 1)
      val json = ujson.write(encode(st.top), indent = indent, sortKeys = sortKeys)
      os.write.over(outputPath, json)

    private def encode(block: Block): ujson.Obj =
      val items = ListBuffer.empty[(String, ujson.Value)]
      items ++= block.counts.view.mapValues(ujson.Num(_))
      items ++= block.times.view.mapValues(t => ujson.Num(t / 1.0e6))
      items ++= block.values
      for l -> bs <- block.blocks do
        items += l -> ujson.Arr.from(bs.map(encode))
        val aggItems: Seq[(String, ujson.Value)] = aggregators.flatMap:
          case AllCount =>
            val ks = bs.flatMap(_.counts.keys).toSet
            for k <- ks yield
              val n = bs.map(_.counts.getOrElse(k, 0)).sum
              block.counts(s"$l/$k") = n
              k -> n
          case AllTime =>
            val ks = bs.flatMap(_.times.keys).toSet
            for k <- ks yield
              val n = bs.map(_.times.getOrElse(k, 0L)).sum
              block.times(s"$l/$k") = n
              k -> n / 1.0e6
        items += s"$l/aggregation" -> ujson.Obj.from(aggItems)
      ujson.Obj.from(items)