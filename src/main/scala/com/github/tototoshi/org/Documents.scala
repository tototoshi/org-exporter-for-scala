package com.github.tototoshi.org

trait Documents {
  sealed abstract class Document
  class Header extends Document

  case class Link(label: String, url: String) extends Document
  case class RichText(text: List[Document]) extends Document
  case class PlainText(text: String) extends Document {
    def +(text2: PlainText): PlainText = PlainText(text + text2.text)
  }

  case class Meta(text: String) extends Document
  case object Hr extends Document
  case object Br extends Document {
    override def toString() = "\n"
  }
  case class H1(text: String) extends Header
  case class H2(text: String) extends Header
  case class H3(text: String) extends Header
  case class H4(text: String) extends Header
  case class H5(text: String) extends Header
  case class H6(text: String) extends Header

  case class LI(text: Document) extends Document
  case class UL(lis: List[LI]) extends Document
  case class OL(lis: List[LI]) extends Document

  case class Example(src: String) extends Document
  case class Source(src: String, lang: String) extends Document

  case class Table(header: Option[List[String]],
    body: List[List[String]],
    footer: Option[List[String]]) extends Document

}

