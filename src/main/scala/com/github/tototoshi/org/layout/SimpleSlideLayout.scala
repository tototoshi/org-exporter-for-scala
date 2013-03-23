package com.github.tototoshi.org.layout

import scala.xml.NodeSeq

trait SimpleSlideLayout extends Layout {

  def apply(content: NodeSeq): NodeSeq = {
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

object SimpleSlideLayout extends SimpleSlideLayout
