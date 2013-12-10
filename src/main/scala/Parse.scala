package jp.co.guru.PostgreSQLAnalyze

import com.typesafe.scalalogging.slf4j.Logging
import scala.Array.canBuildFrom

object Parse extends Logging {
  type IndexedString = Tuple2[String, Int]

  private object RE {
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

  def getPlans() = {
    
  }
}
