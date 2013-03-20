package com.github.tototoshi.org

import scala.util.parsing.combinator._
import scala.xml.{ NodeSeq, Text }
import scala.xml.PrettyPrinter
import java.io.File
import org.apache.commons.io.FileUtils

trait Components {
  sealed abstract class Component
  class Header extends Component

  case class Link(label: String, url: String) extends Component
  case class RichText(text: List[Component]) extends Component
  case class PlainText(text: String) extends Component {
    def +(text2: PlainText): PlainText = PlainText(text + text2.text)
  }

  case class Meta(text: String) extends Component
  case object Hr extends Component
  case object Br extends Component {
    override def toString() = "\n"
  }
  case class H1(text: String) extends Header
  case class H2(text: String) extends Header
  case class H3(text: String) extends Header
  case class H4(text: String) extends Header
  case class H5(text: String) extends Header
  case class H6(text: String) extends Header

  case class LI(text: Component) extends Component
  case class UL(lis: List[LI]) extends Component
  case class OL(lis: List[LI]) extends Component

  case class Example(src: String) extends Component
  case class Source(src: String, lang: String) extends Component

  case class Table(header: Option[List[String]],
    body: List[List[String]],
    footer: Option[List[String]]) extends Component

}

object OrgParser extends RegexParsers with Components {

  override def skipWhitespace = false

  val LINE_SEPARATOR = "\n"

  private val newLine = ("\r"?) ~> "\n"
  private val space = " " | "\t"
  private val eof = """\Z""".r
  private val aster = "*"
  private val metaPrefix = rep(space) ~ "#+"
  private val metaLine: Parser[Component] = (metaPrefix | aster) ~ anyChars ~ newLine ^^ { case _ ~ info ~ _ => Meta(info) }
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

  private def list: Parser[Component] = {
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

  private def hr: Parser[Component] = rep(space) ~ repN(5, "-") ~ rep("-") ~ rep(space) ~ newLine ^^ { case _ => Hr }

  private def table: Parser[Component] = {
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

  private def link: Parser[Link] = {
    def charsInBlock = """[^]]+""".r
    def url = {
      def anyCharsNotWhitespace = """[^\s]+""".r
      """https?://""".r ~ anyCharsNotWhitespace ^^ { case scheme ~ url => Link(scheme + url, scheme + url) }
    }
    def file = {
      between("[[", charsInBlock, "]]") ^^ { case path => Link(path, path) }
    }
    def namedUrl = {
      between("[[", (charsInBlock <~ "][") ~ charsInBlock, "]]") ^^ { case label ~ url => Link(label, url) }
    }
    url | namedUrl | file
  }

  private def example: Parser[Component] = {
    def beginExample = metaPrefix ~ """(?i)begin_example""".r
    def endExample = metaPrefix ~ """(?i)end_example""".r
    def exampleLines = rep(not(endExample) ~> anyChars <~ newLine) ^^ { case xs => xs.mkString("\n") }

    beginExample ~ newLine ~> exampleLines <~ endExample ~ newLine ^^ { case src => Example(src) }
  }

  def sourceClassRule(lang: String) = "sh_" + lang

  private def source: Parser[Component] = {
    def beginSource = metaPrefix ~ """(?i)begin_src""".r
    def lang = anyChars
    def endSource = metaPrefix ~ """(?i)end_src""".r
    def sourceLines = rep(not(endSource) ~> anyChars <~ newLine) ^^ { case xs => xs.mkString("\n") }
    beginSource ~ space ~> lang ~ newLine ~ sourceLines <~ endSource ~ newLine ^^ { case lang ~ _ ~ src => Source(src, lang) }
  }

  private def plainText: Parser[Component] = rep1(not(space) ~> anyChar) ^^ { case xs => PlainText(xs.mkString) }
  private def spaceToken: Parser[Component] = space ^^ { PlainText(_) }
  private def richText: Parser[RichText] = rep(link | plainText | spaceToken) ^^ {
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

  private def lines: Parser[List[Component]] = rep(
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

  def parse(in: String): List[Component] = parseAll(lines, ensureTheLastCharIsNewLine(in)) match {
    case Success(result, _) => {
      result
    }
    case Failure(error, _) => List(PlainText(error.toString))
    case Error(error, _) => List(PlainText(error.toString))
  }

  def parseFile(file: File): List[Component] = parse(FileUtils.readFileToString(file))

  val showTablePF: PartialFunction[Component, NodeSeq] = {
    case Table(rowHeadersOpt, rows, footer) => {
      <table class="table table-bordered table-striped">
        {
          (for (rowHeaders <- rowHeadersOpt) yield {
            <thead>
              <tr>{
                for (rowHeader <- rowHeaders) yield <td>{ rowHeader }</td>
              } </tr>
            </thead>
          }).getOrElse(NodeSeq.Empty)
        }<tbody>{
        for (row <- rows) yield {
          <tr>{
            { for (field <- row) yield <td>{ field }</td> }
          }</tr>
        }
      }</tbody>
      </table>
    }
  }

  val showH1PF: PartialFunction[Component, NodeSeq] = { case H1(text) => <h1>{ text }</h1> }

  def showComponent(component: Component): NodeSeq = {
    def showLI(li: LI): NodeSeq = <li>{ li }</li>

    val pf: PartialFunction[Component, NodeSeq] = {
      case RichText(components) => components.map(showComponent).flatten
      case PlainText(text) => Text(text)
      case Example(src) => <pre>{ src }</pre>
      case Source(src, lang) => <pre class={ "sh_" + lang }>{ src }</pre>
      case Link(url, label) => <a href={ url }>{ label }</a>
      case H2(text) => <h2>{ text }</h2>
      case H3(text) => <h3>{ text }</h3>
      case H4(text) => <h4>{ text }</h4>
      case H5(text) => <h5>{ text }</h5>
      case H6(text) => <h6>{ text }</h6>
      case LI(li) => showComponent(li)
      case UL(lis) => <ul>{ for (li <- lis) yield <li>{ showComponent(li) }</li> }</ul>
      case OL(lis) => <ol>{ for (li <- lis) yield <li>{ showComponent(li) }</li> }</ol>
      case Br => <br/>
      case Hr => <hr/>
      case Meta(info) => NodeSeq.Empty
    }
    val f = pf.orElse(showTablePF).orElse(showH1PF)

    f(component)

  }

  def main(args: Array[String]): Unit = {

    case class Config(mode: String, inputFile: File, outputFile: File)

    val parser = new scopt.immutable.OptionParser[Config]("org-export", "0.1.0") { def options = Seq(
      flag("slide", "slide mode") { (c: Config) => c.copy(mode = "slide") },
      flag("html", "html mode") { (c: Config) => c.copy(mode = "html") },
      opt("o", "out", "output") { (v: String, c: Config) => c.copy(outputFile = new File(v)) },
      arg("<file>", "input file") { (v: String, c: Config) => c.copy(inputFile = new File(v)) }
    ) }

    def validate(config: Config): Either[String, Config] = {
      if (config.inputFile == null) {
        Left("input file required")
      } else if (config.outputFile == null) {
        Left("out file required")
      } else if (config.mode != "html" && config.mode != "slide") {
        Left("output mode should be either 'html' or 'slide'.")
      } else {
        Right(config)
      }
    }

    parser.parse(args, Config(null, null, null)).map { config =>
      validate(config) match {
        case Right(c) => {
          val result = parseFile(c.inputFile).map { showComponent _ }.flatten

          c.mode match {
            case "slide" => {
              FileUtils.writeStringToFile(c.outputFile, slideLayout(result).toString)
            }
            case "html" => {
              FileUtils.writeStringToFile(c.outputFile, bootStrapLayout(result).toString)
            }
          }
        }
        case Left(errorMsg) => {
          println(errorMsg)
        }
      }
    } getOrElse {
      println("Invalid argument.")
    }
  }

  def bootStrapLayout(content: NodeSeq): NodeSeq =
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
          { content }
        </div>
        <script type="text/javascript">
          sh_highlightDocument();
        </script>
      </body>
    </html>

  def slideLayout(content: NodeSeq): NodeSeq = {
    var result = NodeSeq.Empty

    var section = NodeSeq.Empty

    content.toIterable.dropWhile(_.isEmpty).foreach { n =>

      n match {
        case h @ <h1>{ text }</h1> => {
          if (!section.isEmpty) {
            result ++= <section>{ section }</section>
            section = NodeSeq.Empty
          }
          section ++= h
        }
        case x => {
          section ++= x
        }
      }
    }

    result ++= { <section>{ section }</section> }

    <html lang="ja">
      <head>
        <meta charset="utf-8"/>
        <title></title>
        <script type="text/javascript" src="js/jquery.js"></script>
        <script type="text/javascript" src="js/main.js"></script>
        <link rel="stylesheet" href="css/style.css"/>
        <link rel="stylesheet" href="css/custom.css"/>
        <script type="text/javascript" src="js/custom.js"></script>
        <script type="text/javascript" src="http://shjs.sourceforge.net/lang/sh_scala.js"></script>
        <link type="text/css" rel="stylesheet" media="screen,projection" href="shjs-0.6/css/sh_bright.min.css"/>
        <script type="text/javascript" src="shjs-0.6/sh_main.min.js"></script>
      </head>
      <body onload="sh_highlightDocument('shjs-0.6/lang/', '.js');">
        { result }
      </body>
    </html>

  }

}

