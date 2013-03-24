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
