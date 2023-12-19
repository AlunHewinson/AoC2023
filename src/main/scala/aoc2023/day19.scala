package aoc2023

import utils.utils.readDay

import scala.annotation.tailrec

object day19 extends App {
  // https://adventofcode.com/2023/day/19

  def solveDay(day: Int, test: String = ""): (BigInt, BigInt) = {

    val inp: String = readDay(day, test, year = 2023)
    val sectionSeparator: String = "(\r*\n){2}"
    val sections: Seq[String] = sectionSeparator.r.split(inp)

    val lineSeparator: String = "\r*\n"
    val rawWorkflows: Array[String] = lineSeparator.r.split(sections.head)
    val workflows = rawWorkflows.map(w => Workflow.apply(w))
    val rawInputs: Array[String] = lineSeparator.r.split(sections.last)
    val inputs: Seq[Input] = rawInputs.map(i => Input.apply(i))

    @tailrec
    def process1(input: Input, workflowName: String, workflows: Seq[Workflow]): String = {
      if (workflowName == "A") workflowName
      else if (workflowName == "R") workflowName
      else {
        process1(input, workflows.filter(w => w.name == workflowName).head.process(input) , workflows)
      }
    }
    val outputs = inputs.map(i => process1(i, "in", workflows))
    val inputSums = outputs.zip(inputs).filter(x => {
      x._1 == "A"
    }).map(_._2.sum())

    val answer1 = inputSums.sum
    val initialFourRange: FourRange = FourRange("in", 1 to 4000, 1 to 4000, 1 to 4000, 1 to 4000)

    @tailrec
    def process2a(acc: Seq[FourRange] = Seq()): Seq[FourRange] = {
      val finalised = acc.filter(fr => Seq("A", "R").contains(fr.node))
      val fourRanges = acc.filterNot(fr => Seq("A", "R").contains(fr.node))
      if (fourRanges.isEmpty) acc
      else process2a(fourRanges.flatMap(x => x.divide(workflows)) ++ finalised)
    }
    val splitted = process2a(Seq(initialFourRange))

    val answer2 = splitted
      .filter(_.node == "A")
      .map(q => BigInt(q.x.length) * BigInt(q.m.length) * BigInt(q.a.length) * BigInt(q.s.length))

    (answer1, answer2.sum)

  }

  println(solveDay(day = 19, test = "test"))
  println(solveDay(day = 19))

}

case class Condition(dimension: String, gt: String, x: Int) {
  def resolve(input: Input): Boolean = {
    val inputValue: Int = dimension match {
      case "x" => input.x
      case "m" => input.m
      case "a" => input.a
      case "s" => input.s
      case _   => 0
    }
    gt match {
      case ">" => inputValue > x
      case "<" => inputValue < x
      case _   => true
    }
  }
  def show(): Unit = {
    println(s"  c: $dimension $gt $x")
  }
}
object Condition {
  def apply(conditionString: String): Condition = {
    val dimension = "^[A-Za-z]+".r.findFirstIn(conditionString).get
    val gt = "[^A-Za-z0-9]+".r.findFirstIn(conditionString).get
    val x = "[0-9]+$".r.findFirstIn(conditionString).get.toInt
    new Condition(dimension, gt, x)
  }
}
case class Rule(condition: Condition, target: String) {
  def follow(input: Input): Option[String] = {
    if (this.condition.resolve(input)) Some(target)
    else None
  }
  def show(): Unit = {
    condition.show()
    println(s"  t: $target")
  }
}
object Rule {
  def apply(ruleString: String): Rule = {
    if (".*:.*".r.matches(ruleString)) {
      val conditionTarget = ":".r.split(ruleString)
      new Rule(Condition(conditionTarget.head), conditionTarget.last)
    }
    else {
      new Rule(Condition("zzz", "zzz", 5000), ruleString)
    }
  }
}
case class Workflow(name: String, rules: Seq[Rule]) {
  @tailrec
  final def process(input: Input, ruleSeq: Seq[Rule] = this.rules): String = {
    if (ruleSeq.isEmpty) ""
    else {
      val oneRuleResult: Option[String] = ruleSeq.head.follow(input)
      if (oneRuleResult.isDefined) oneRuleResult.get
      else this.process(input, ruleSeq.tail)
    }
  }
  def show(): Unit = {
    println(s"w: $name")
    rules.foreach(_.show())
  }
}
object Workflow {
  def apply(workflowString: String): Workflow = {
    val workflowName = "^[a-z]+".r.findFirstIn(workflowString).get
    val workFlowRulesString = "\\{(.*?)}".r.findAllIn(workflowString).matchData.map(_.group(1)).toSeq.head
    val rules: Seq[Rule] = ",".r.split(workFlowRulesString).map(x => Rule(x))
    new Workflow(workflowName, rules)
  }
}
case class Input(x: Int, m: Int, a: Int, s: Int) {
  def show(): Unit = {
    println(s"x: $x  m: $m  a: $a  s: $s")
  }
  def sum(): Int = {
    this.x + this.m + this.a + this.s
  }
}
object Input {
  def apply(inputString: String): Input = {
    val xmas: Seq[Int] = "[0-9]+".r.findAllIn(inputString).toSeq.map(_.toInt)
    new Input(x = xmas.head, m = xmas(1), a = xmas(2), s = xmas(3))
  }
}

case class FourRange(node: String, x: Range, m: Range, a: Range, s: Range) {

  def show(): Unit = {
    println(s"${this.node}: x=$x, m=$m, a=$a, s=$s")
  }

  private def divide(rule: Rule): Seq[FourRange] = {
    // Rule  -->  condition: Condition, target: String
    //                       Condition  -->  dimension: String, gt: String, x: Int
    // find the dimension and split the "true" element out into a FourRange with the node from the Rule

    val within = rule.condition.dimension

    val lowerUpperAt = rule.condition.gt match {
      case "<" => (rule.target, "", rule.condition.x)
      case ">" => ("", rule.target, rule.condition.x + 1)
      case _   => ("", rule.target, -98)
    }

    val lowerNode = lowerUpperAt._1
    val upperNode = lowerUpperAt._2
    val at = lowerUpperAt._3

    def isInclusiveRange(r: Range): Boolean = {
      r.length > r.end - r.start
    }

    within match {
      case "x" if at < x.start => Seq(this)
      case "x" if at > x.end => Seq(this)
      case "x" =>
        if (isInclusiveRange(x)) {
          Seq(this.copy(x = this.x.start until at, node = lowerNode), this.copy(x = at to this.x.end, node = upperNode))
        } else {
          Seq(this.copy(x = this.x.start until at, node = lowerNode), this.copy(x = at until this.x.end, node = upperNode))
        }

      case "m" if at < m.start => Seq(this)
      case "m" if at > m.end => Seq(this)
      case "m" =>
        if (isInclusiveRange(m)) {
          Seq(this.copy(m = this.m.start until at, node = lowerNode), this.copy(m = at to this.m.end, node = upperNode))
        } else {
          Seq(this.copy(m = this.m.start until at, node = lowerNode), this.copy(m = at until this.m.end, node = upperNode))
        }

      case "a" if at < a.start => Seq(this)
      case "a" if at > a.end => Seq(this)
      case "a" =>
        if (isInclusiveRange(a)) {
          Seq(this.copy(a = this.a.start until at, node = lowerNode), this.copy(a = at to this.a.end, node = upperNode))
        } else {
          Seq(this.copy(a = this.a.start until at, node = lowerNode), this.copy(a = at until this.a.end, node = upperNode))
        }

      case "s" if at < s.start => Seq(this)
      case "s" if at > s.end   => Seq(this)
      case "s"                 =>
        if (isInclusiveRange(s)) {
          Seq(this.copy(s = this.s.start until at, node = lowerNode), this.copy(s = at to this.s.end, node = upperNode))
        } else {
          Seq(this.copy(s = this.s.start until at, node = lowerNode), this.copy(s = at until this.s.end, node = upperNode))
        }

      case _                   => Seq(this.copy(node = upperNode))

    }

  }

  private def divide(workflow: Workflow): Seq[FourRange] = {
    val divided: Seq[FourRange] = this.divide(workflow.rules.head)
    val newWorkflow: Workflow = workflow.copy(rules = workflow.rules.tail)
    val unnamedFourRanges: Seq[FourRange] = divided.filter(_.node == "")
    val namedFourRanges: Seq[FourRange] = divided.filterNot(_.node == "")
    val redivided: Seq[FourRange] = unnamedFourRanges.flatMap(_.divide(newWorkflow))
    val toReturn: Seq[FourRange] = redivided ++ namedFourRanges

    toReturn
  }

  def divide(workflows: Seq[Workflow]): Seq[FourRange] = {
    val relevantWorkflow: Workflow = workflows.filter(_.name == this.node).head
    val yoyo: Seq[FourRange] = divide(relevantWorkflow)
    yoyo
  }

}