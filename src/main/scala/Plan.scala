package jp.co.guru.PostgreSQLAnalyze

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

case class Cost(start: Double, end: Double, row: Int, width: Int) extends CostBase {
  protected def additional() = {
    List(width.toString)
  }
}

case class ActualTime(start: Double, end: Double, row: Int, loop: Int) extends CostBase {
  protected def additional() = {
    List(loop.toString)
  }
}

trait CostBase {
  def start: Double
  def end: Double
  def row: Int
  protected def additional: List[String]
  def elapse() = end - start
  def csv() = List(start.toString, end.toString, elapse, row.toString) ++ additional
  def excel(sum: CostBase): List[String] = {
    List(start.toString, end.toString, elapse.toString, (elapse / sum.end * 100).toString, row.toString) ++ additional
  }
}
