package com.github.tototoshi.org.layout

import scala.xml.NodeSeq

trait Layout {

  def apply(content: NodeSeq): NodeSeq

}

