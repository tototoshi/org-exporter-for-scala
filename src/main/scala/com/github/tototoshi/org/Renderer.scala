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

import scala.xml.{ NodeSeq, Text }

trait Renderer extends Documents {

  val showTablePF: PartialFunction[Document, NodeSeq] = {
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

  val showH1PF: PartialFunction[Document, NodeSeq] = { case H1(text) => <h1>{ text }</h1> }

  def render(document: Document): NodeSeq = {
    def showLI(li: LI): NodeSeq = <li>{ li }</li>

    val pf: PartialFunction[Document, NodeSeq] = {
      case RichText(documents) => documents.map(render).flatten
      case PlainText(text) => Text(text)
      case Example(src) => <pre>{ src }</pre>
      case Source(src, lang) => <pre class={ "sh_" + lang }>{ src }</pre>
      case Link(url, label) => <a href={ url }>{ label }</a>
      case Image(src) => <img src={ src }/>
      case H2(text) => <h2>{ text }</h2>
      case H3(text) => <h3>{ text }</h3>
      case H4(text) => <h4>{ text }</h4>
      case H5(text) => <h5>{ text }</h5>
      case H6(text) => <h6>{ text }</h6>
      case LI(li) => render(li)
      case UL(lis) => <ul>{ for (li <- lis) yield <li>{ render(li) }</li> }</ul>
      case OL(lis) => <ol>{ for (li <- lis) yield <li>{ render(li) }</li> }</ol>
      case Br => <br/>
      case Hr => <hr/>
      case Meta(info) => NodeSeq.Empty
    }
    val f = pf.orElse(showTablePF).orElse(showH1PF)

    f(document)

  }
}
