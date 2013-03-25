/*
 * Copyright 2013 Toshiyuki Takahashi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.github.tototoshi.org

import scala.util.parsing.combinator._
import scala.xml.{ NodeSeq, Text }
import scala.xml.PrettyPrinter
import java.io.File
import org.apache.commons.io.FileUtils

trait Parser extends RegexParsers with Documents {

  override def skipWhitespace = false

  val LINE_SEPARATOR = "\n"

  private val newLine = ("\r"?) ~> "\n"
  private val space = " " | "\t"
  private val eof = """\Z""".r
  private val aster = "*"
  private val metaPrefix = rep(space) ~ "#+"
  private val metaLine: Parser[Document] = (metaPrefix | aster) ~ anyChars ~ newLine ^^ { case _ ~ info ~ _ => Meta(info) }
  private val anyChar = """.""".r

  def between[T](open: String, parser: Parser[T], close: String): Parser[T] = open ~> parser <~ close

  private def blankLine = newLine ^^ { _ => Br }

  private def anyChars = rep(anyChar) ^^ { case x => x.mkString }

  private def header: Parser[Header] = {
    def h1 = aster ~ space ~> anyChars <~ newLine ^^ { s => H1(s) }
    def h2 = repN(2, aster) ~ space ~> anyChars <~ newLine ^^ { s => H2(s) }
    def h3 = repN(3, aster) ~ space ~> anyChars <~ newLine ^^ { s => H3(s) }
    def h4 = repN(4, aster) ~ space ~> anyChars <~ newLine ^^ { s => H4(s) }
    def h5 = repN(5, aster) ~ space ~> anyChars <~ newLine ^^ { s => H5(s) }
    def h6 = repN(6, aster) ~ space ~> anyChars <~ newLine ^^ { s => H6(s) }
    h1 | h2 | h3 | h4 | h5 | h6
  }

  private def list: Parser[Document] = {
    def ul = {
      def li = rep(space) ~ "-" ~ rep(space) ~> richText <~ newLine ^^ { case x => LI(x) }
      rep1(li) ^^ { x => OL(x) }
    }
    def ol = {
      def li = rep(space) ~ """[0-9]""".r ~ "." ~ rep(space) ~> richText <~ newLine ^^ { case x => LI(x) }
      rep1(li) ^^ { x => OL(x) }
    }
    ul | ol
  }

  private def hr: Parser[Document] = rep(space) ~ repN(5, "-") ~ rep("-") ~ rep(space) ~ newLine ^^ { case _ => Hr }

  private def table: Parser[Document] = {
    def separator = rep(space) ~ "|" ~> rep1sep("""-+""".r, "+") <~ "|" ~ rep(space) ~ newLine

    def th = rep(space) ~ "|" ~> rep1sep("""[^|]+""".r, "|" ~ not(newLine)) <~ "|" ~ rep(space) ~ newLine

    def td = rep(space) ~ "|" ~> rep1sep("""[^|]+""".r, "|" ~ not(newLine)) <~ "|" ~ rep(space) ~ newLine

    val header = opt(separator ~> th <~ separator)
    val body = rep1(not(separator) ~> td)
    val footer = opt(th <~ separator)
    (header ~ body <~ opt(separator)) ~ footer ^^ {
      case xs ~ ys ~ zs => Table(xs, ys, zs)
    }

  }

  private def image: Parser[Image] = {
    def charsInBlock = """[^]]+""".r
    between("[[file:", charsInBlock, "]]") ^^ { case path => Image(path) }
  }

  private def link: Parser[Link] = {
    def charsInBlock = """[^]]+""".r
    def url = {
      def anyCharsNotWhitespace = """[^\s]+""".r
      """https?://""".r ~ anyCharsNotWhitespace ^^ { case scheme ~ url => Link(scheme + url, scheme + url) }
    }
    def namedUrl = {
      between("[[", (charsInBlock <~ "][") ~ charsInBlock, "]]") ^^ { case label ~ url => Link(label, url) }
    }
    url | namedUrl
  }

  private def example: Parser[Document] = {
    def beginExample = metaPrefix ~ """(?i)begin_example""".r
    def endExample = metaPrefix ~ """(?i)end_example""".r
    def exampleLines = rep(not(endExample) ~> anyChars <~ newLine) ^^ { case xs => xs.mkString("\n") }

    beginExample ~ newLine ~> exampleLines <~ endExample ~ newLine ^^ { case src => Example(src) }
  }

  def sourceClassRule(lang: String) = "sh_" + lang

  private def source: Parser[Document] = {
    def beginSource = metaPrefix ~ """(?i)begin_src""".r
    def lang = anyChars
    def endSource = metaPrefix ~ """(?i)end_src""".r
    def sourceLines = rep(not(endSource) ~> anyChars <~ newLine) ^^ { case xs => xs.mkString("\n") }
    beginSource ~ space ~> lang ~ newLine ~ sourceLines <~ endSource ~ newLine ^^ { case lang ~ _ ~ src => Source(src, lang) }
  }

  private def plainText: Parser[Document] = rep1(not(space) ~> anyChar) ^^ { case xs => PlainText(xs.mkString) }
  private def spaceToken: Parser[Document] = space ^^ { PlainText(_) }
  private def richText: Parser[RichText] = rep(link | image | plainText | spaceToken) ^^ {
    case xs => RichText(xs)
  }

  private def paragraph: Parser[RichText] = {
    def line = not(blankLine | header | hr | list | metaLine | table) ~> richText <~ newLine
    rep1(line) ^^ {
      case lines =>
        lines.foldLeft(RichText(Nil)) { (x: RichText, y: RichText) =>
          RichText(x.text ::: y.text)
        }
    }
  }

  private def lines: Parser[List[Document]] = rep(
    header
      | hr
      | list
      | blankLine
      | table
      | example
      | source
      | metaLine
      | paragraph
  )

  private def ensureTheLastCharIsNewLine(s: String): String =
    if (!s.endsWith(LINE_SEPARATOR)) s + LINE_SEPARATOR else s

  def parse(in: String): List[Document] = parseAll(lines, ensureTheLastCharIsNewLine(in)) match {
    case Success(result, _) => {
      result
    }
    case Failure(error, _) => List(PlainText(error.toString))
    case Error(error, _) => List(PlainText(error.toString))
  }

  def parseFile(file: File): List[Document] = parse(FileUtils.readFileToString(file))



}


