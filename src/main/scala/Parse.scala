import com.typesafe.scalalogging.slf4j.Logging
import scala.Array.canBuildFrom

object Parse extends Logging {
  type IndexedString = Tuple2[String, Int]

  trait CostBase {
    def s: Double
    def e: Double
    def row: Int
    protected def additional: List[String]
    def elapse() = e - s
    def csv() = List(s.toString, e.toString, elapse, row.toString) ++ additional
    def excel(sum: CostBase): List[String] = {
      List(s.toString, e.toString, elapse.toString, (elapse / sum.e * 100).toString, row.toString) ++ additional
    }
  }

  case class Cost(s: Double, e: Double, row: Int, width: Int) extends CostBase {
    protected def additional() = {
      List(width.toString)
    }
  }

  case class ActualTime(s: Double, e: Double, row: Int, loop: Int) extends CostBase {
    protected def additional() = {
      List(loop.toString)
    }
  }

  case class Plan(line: Int, op: String, cost: Cost, actualTime: ActualTime, desc: List[String]) {
    def csv(sum: Plan) = {
      op +: List(cost.csv, actualTime.csv, desc).flatten
    }
    def excelHeader() = {
      List("No", "Op", "CS", "CE", "elapse", "CRatio", "CRow", "Width", "AS", "AE", "elapse", "ARatio", "ARow", "Loop").mkString("\t")
    }
    def excel(sum: Plan): List[String] = {
      val dataList = List(cost.excel(sum.cost), actualTime.excel(sum.actualTime)).flatten
      (line + 1).toString +: op +: dataList :+ desc.mkString("\"", "\n", "\"")
    }
  }

  object RE {
    val FP = """[0-9]+\.[0-9]+"""
    val INT = "[0-9]+"
    val FPRANGE = s"($FP)\\.\\.($FP)"
    def COST(s: String) = s"\\($s=$FPRANGE rows=($INT) (?:width|loops)=($INT)\\)"
    val TimeRE = ("""(?:\s*->\s*)?(.*?)\s*""" +
      COST("cost") + "\\s+" +
      COST("actual time")).r
  }

  class ResultIterator(s: String) extends Iterator[Plan] {
    val rawLines = s.split("\r\n").zipWithIndex.filter(_._1.length != 0)
    val HR = "\"\\s*(.*)\"".r
    val lines: Array[Either[IndexedString, IndexedString]] = rawLines.map(l => {
      l._1 match {
        case HR(b) => Right(b, l._2)
        case x     => Left("Not match line" + l._2 + " " + l._1, l._2)
      }
    })
    //    lines.foreach(l => println("[" + l + "]"))

    // 終了判定の為、先読みが必要（なのでBufferedIterator)
    val source = lines.iterator.buffered

    def hasNext() = { source.hasNext }
    def next() = {
      getNext()
    }

    // プランを1つ読み込む
    // 次のプランが現れるまで　付随する情報として
    def getNext(): Plan = {

      // 付随情報の読み込み
      def getDesc(value: List[String] = Nil): List[String] = {
        if (!source.hasNext) {
          value
        } else {
          source.head match {
            case Left(x) =>
              source.next; getDesc(x._1 +: value)
            case Right(t) =>
              t._1 match {
                case RE.TimeRE(op, cs, ce, row1, width, as, ae, row2, loop) => value
                case s: String => source.next; getDesc(s +: value)
              }
          }
        }
      }

      assert(source.hasNext, "予期せぬEOF")
      source.head match {
        case Right(tp) =>
          tp._1 match {
            case RE.TimeRE(op, cs, ce, row1, width, as, ae, row2, loop) =>
              val cost = Cost(cs.toDouble, ce.toDouble, row1.toInt, width.toInt)
              val actualTime = ActualTime(as.toDouble, ae.toDouble, row2.toInt, loop.toInt)
              source.next;
              Plan(tp._2, op, cost, actualTime, getDesc().reverse)
            case _ => throw new RuntimeException("")
          }
        case Left(x) => println("GN"); source.next; getNext()
      }
    }
  }

}
