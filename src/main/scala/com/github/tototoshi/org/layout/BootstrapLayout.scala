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

trait BootstrapLayout extends Layout {

  def apply(content: NodeSeq): NodeSeq =
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

}

object BootstrapLayout extends BootstrapLayout
