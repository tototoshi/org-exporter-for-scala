package com.github.tototoshi.org

import scala.util.parsing.combinator._
import scala.xml.{ NodeSeq, Text }
import scala.xml.PrettyPrinter
import java.io.File

object OrgParser extends RegexParsers {

  override def skipWhitespace = false

  val LINE_SEPARATOR = "\n"

  private val newLine = ("\r"?) ~> "\n"
  private val space = " " | "\t"
  private val eof = """\Z""".r
  private val aster = "*"
  private val metaPrefix = rep(space) ~ "#+"
  private val metaLine = (metaPrefix | aster) ~ anyChars ~ newLine ^^ { _ => NodeSeq.Empty }
  private val anyChar = """.""".r

  private def blankLine = newLine ^^ { _ => NodeSeq.Empty }

  private def anyChars = rep(anyChar) ^^ { _.mkString }

  private def header = {
    def id = scala.util.Random.alphanumeric.take(10).mkString
    def h1 = aster ~ space ~> anyChars <~ newLine ^^ { a => <h1 id={ id }>{ a }</h1> }
    def h2 = repN(2, aster) ~ space ~> anyChars <~ newLine ^^ { a => <h2 id={ id }>{ a }</h2> }
    def h3 = repN(3, aster) ~ space ~> anyChars <~ newLine ^^ { a => <h3 id={ id }>{ a }</h3> }
    def h4 = repN(4, aster) ~ space ~> anyChars <~ newLine ^^ { a => <h4 id={ id }>{ a }</h4> }
    def h5 = repN(5, aster) ~ space ~> anyChars <~ newLine ^^ { a => <h5 id={ id }>{ a }</h5> }
    def h6 = repN(6, aster) ~ space ~> anyChars <~ newLine ^^ { a => <h6 id={ id }>{ a }</h6> }
    h1 | h2 | h3 | h4 | h5 | h6
  }

  private def list = {
    def ul = {
      def li = rep(space) ~ "-" ~ rep(space) ~> richText <~ newLine
      rep1(li) ^^ { lis => <ul>{ for (li <- lis) yield <li>{ li }</li> }</ul> }
    }
    def ol = {
      def li = rep(space) ~ """[0-9]""".r ~ "." ~ rep(space) ~> richText <~ newLine
      rep1(li) ^^ { lis => <ol>{ for (li <- lis) yield <li>{ li }</li> }</ol> }
    }
    ul | ol
  }

  private def hr = rep(space) ~ repN(5, "-") ~ rep("-") ~ rep(space) ~ newLine ^^ { _ => <hr/> }

  val tableClass = "table table-bordered table-striped"

  private def table = {
    def separator = rep(space) ~ "|" ~> rep1sep("""-+""".r, "+") <~ "|" ~ rep(space) ~ newLine

    def td = rep(space) ~ "|" ~> rep1sep("""[^|]+""".r, "|" ~ not(newLine)) <~ "|" ~ rep(space) ~ newLine ^^ { tds =>
      <tr>{ for (td <- tds) yield <td>{ td.trim }</td> }</tr>
    }

    def th = rep(space) ~ "|" ~> rep1sep("""[^|]+""".r, "|" ~ not(newLine)) <~ "|" ~ rep(space) ~ newLine ^^ { tds =>
      <tr>{ for (td <- tds) yield <th>{ td.trim }</th> }</tr>
    }

    opt(separator ~> th <~ separator) ~
      rep1(not(separator) ~> td) ~
      opt(separator) ~ opt(th <~ separator) ^^ {
        case th ~ trs ~ tf ~ tf2 => {
          <table class={ tableClass }>
            { th.map { x => <thead>{ x }</thead> }.getOrElse(NodeSeq.Empty) }
            <tbody>{ trs }</tbody>
            { tf2.map { x => <tfoot>{ x }</tfoot> }.getOrElse(NodeSeq.Empty) }
          </table>
        }
      }

  }

  private def link = {
    def charsInBlock = """[^]]+""".r
    def url = {
      def anyCharsNotWhitespace = """[^\s]+""".r
      """https?://""".r ~ anyCharsNotWhitespace ^^ {
        case a ~ b => <a href={ a + b }>{ a + b }</a>
      }
    }
    def file = {
      "[[" ~> charsInBlock <~ "]]" ^^ {
        case a => <a href={ a }>{ a }</a>
      }
    }
    def namedUrl = {
      "[[" ~ charsInBlock ~ "][" ~ charsInBlock ~ "]]" ^^ {
        case _ ~ a ~ _ ~ b ~ _ => <a href={ a }>{ b }</a>
      }
    }
    url | namedUrl | file
  }

  private def example = {
    def beginExample = metaPrefix ~ """(?i)begin_example""".r
    def endExample = metaPrefix ~ """(?i)end_example""".r
    def exampleLines = rep(not(endExample) ~> anyChars <~ newLine) ^^ { case lines => lines.mkString("\n") }

    beginExample ~ newLine ~> exampleLines <~ endExample ~ newLine ^^ {
      case e => <pre>{ e }</pre>
    }
  }

  def sourceClassRule(lang: String) = "sh_" + lang

  private def source = {
    def beginSource = metaPrefix ~ """(?i)begin_src""".r
    def lang = anyChars
    def endSource = metaPrefix ~ """(?i)end_src""".r
    def sourceLines = rep(not(endSource) ~> anyChars <~ newLine) ^^ { case lines => lines.mkString("\n") }
    beginSource ~ space ~> lang ~ newLine ~ sourceLines <~ endSource ~ newLine ^^ {
      case lang ~ nl ~ e => <pre class={ sourceClassRule(lang) }>{ e }</pre>
    }
  }

  private def plainText = rep1(anyChar) ^^ { _.mkString }
  private def richText = rep(link | plainText | space)

  private def paragraph = {
    def line = not(blankLine | header | hr | list | metaLine | table) ~> richText <~ newLine
    rep1(line) ^^ { case e => <p>{ e }</p> }
  }

  private def lines = rep(
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

  def parse(in: String): NodeSeq = parseAll(lines, ensureTheLastCharIsNewLine(in)) match {
    case Success(result, _) => {
      val html = result.flatten

      val menu = {
        var h1Index = 0
        var h2Index = 0
        var h3Index = 0

        html.foldLeft(NodeSeq.Empty) { (x, y) =>
          x ++ (y match {
            case h @ <h1>{ label }</h1> => {
              h1Index += 1
              h2Index = 0
              h3Index = 0
              val anchor = "#" + (h \ "@id" text)
              <li class="h1"><a href={ anchor }>{ h1Index + " " + label }</a></li>
            }
            case h @ <h2>{ label }</h2> => {
              h2Index += 1
              h3Index = 0
              val anchor = "#" + (h \ "@id" text)
              <li class="h2"><a href={ anchor }>{ List(h1Index, h2Index).mkString(".") + " " + label }</a></li>
            }
            case h @ <h3>{ label }</h3> => {
              h3Index += 1
              val anchor = "#" + (h \ "@id" text)
              <li class="h3"><a href={ anchor }>{ List(h1Index, h2Index, h3Index).mkString(".") + " " + label }</a></li>
            }
            case _ => NodeSeq.Empty
          })
        }
      }

      val tableOfContents = (<ul>{ menu }</ul>)

      <html>
        <head>
          <title>Exported html</title>
          <script type="text/javascript" src="http://shjs.sourceforge.net/sh_main.min.js"></script>
          <script type="text/javascript" src="http://shjs.sourceforge.net/lang/sh_scala.js"></script>
          <script type="text/javascript" src="http://shjs.sourceforge.net/lang/sh_html.js"></script>
          <script type="text/javascript" src="http://shjs.sourceforge.net/lang/sh_sql.js"></script>
          <link href="http://shjs.sourceforge.net/css/sh_darkness.min.css" rel="stylesheet"></link>
          <link type="text/css" rel="stylesheet" media="screen,projection" href="http://bootswatch.com/default/bootstrap.min.css"></link>
          <style type="text/css" media="screen">
            body {{ padding-top: 60px; }}
            code {{ background-color: #fff; border: none; }}
            h1, h2, h3, h4 {{ padding-top:50px; margin-top:-50px; }}
          </style>
        </head>
        <body>
          <div class="navbar navbar-fixed-top">
            <div class="navbar-inner">
              <div class="container">
                <a class="brand" href="/">Document</a>
              </div>
            </div>
          </div>
          <div class="container">
            { tableOfContents ++ html }
          </div>
          <script type="text/javascript">
            sh_highlightDocument();
          </script>
        </body>
      </html>

    }
    case Failure(error, _) => Text(error.toString)
    case Error(error, _) => Text(error.toString)
  }

  def parseFile(file: File): NodeSeq = {
    val src = scala.io.Source.fromFile(file)
    val content = try {
      src.mkString
    } finally {
      src.close()
    }
    parse(content)
  }

}

