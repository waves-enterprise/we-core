package com.wavesenterprise.transaction.generator.base

/**
  * Abstract functional code writer with convenient formatting tools
  */
case class CodeWriter(content: Vector[String] = Vector.empty, indentLevel: Int = 0) {
  import CodeWriter._

  def indent: CodeWriter = indent(1)

  def indent(n: Int): CodeWriter = copy(indentLevel = indentLevel + n)

  def outdent: CodeWriter = outdent(1)

  def outdent(n: Int): CodeWriter = {
    require(indentLevel >= n)
    copy(indentLevel = indentLevel - n)
  }

  def append(s: String): CodeWriter = {
    copy(content = content.updated(content.size - 1, content.last + s))
  }

  def addLines(lines: String*): CodeWriter = {
    val trueLines     = lines.flatMap(_.split("\n", -1))
    val indentedLines = trueLines.map(line => IndentSymbol * (indentLevel * IndentSize) + line)
    copy(content = content ++ indentedLines)
  }

  def addLinesIndented(lines: String*): CodeWriter = {
    indent.addLines(lines: _*).outdent
  }

  def callIndented(f: EndoFunctor*): CodeWriter = indent.call(f: _*).outdent

  def newLine: CodeWriter = addLines("")

  def addStringMargin(s: String): CodeWriter = addLines(s.stripMargin)

  def addStringMarginIndented(s: String): CodeWriter = addLinesIndented(s.stripMargin)

  def call(f: EndoFunctor*): CodeWriter = f.foldLeft(this)((p, f) => f(p))

  def applyIf(cond: => Boolean)(f: EndoFunctor): CodeWriter = {
    if (cond) f(this)
    else this
  }

  def fold[T](items: Iterable[T])(f: (CodeWriter, T) => CodeWriter): CodeWriter = {
    items.foldLeft(this)(f)
  }

  def foldWithAccumulator[T, A](items: Iterable[T], acc: A)(f: ((CodeWriter, A), T) => (CodeWriter, A)): CodeWriter = {
    val (writer, _) = items.foldLeft(this -> acc)(f)
    writer
  }

  def foldIndented[T](items: Iterable[T])(f: (CodeWriter, T) => CodeWriter): CodeWriter = {
    indent.fold(items)(f).outdent
  }

  def foldWithAccumulatorIndented[T, A](items: Iterable[T], acc: A)(f: ((CodeWriter, A), T) => (CodeWriter, A)): CodeWriter = {
    val (writer, _) = items.foldLeft(this.indent -> acc)(f)
    writer.outdent
  }

  def foldWithDelimiter[T](objects: Iterable[T], delimiter: String)(f: (CodeWriter, T) => CodeWriter): CodeWriter = {
    val first = objects.headOption.fold(this)(f(this, _))
    objects.drop(1).foldLeft(first) {
      case (acc, item) =>
        val separated = acc.append(delimiter)
        f(separated, item)
    }
  }

  def foldWithAccumulatorAndDelimiterIndented[T, A](items: Iterable[T], acc: A, delimiter: String)(
      f: ((CodeWriter, A), T) => (CodeWriter, A)
  ): CodeWriter = {
    val first = items.headOption.fold(this.indent -> acc)(f(this.indent -> acc, _))
    val (writer, _) = items.drop(1).foldLeft(first) {
      case ((w, acc), i) =>
        val separated = w.append(delimiter)
        f((separated, acc), i)
    }
    writer.outdent
  }

  def foldWithDelimiterIndented[T](objects: Iterable[T], delimiter: String)(f: (CodeWriter, T) => CodeWriter): CodeWriter = {
    indent.foldWithDelimiter(objects, delimiter)(f).outdent
  }

  def build(): String = content.mkString("\n")

  def combine(other: CodeWriter): CodeWriter = {
    new CodeWriter(content ++ other.content, indentLevel + other.indentLevel)
  }

  override def toString: String = s"CodeWriter(lines=${content.length}, indentLevel=$indentLevel)"
}

object CodeWriter {
  type EndoFunctor = CodeWriter => CodeWriter

  val IndentSymbol = " "
  val IndentSize   = 2
}
