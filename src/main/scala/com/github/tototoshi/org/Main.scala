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

import java.io.File
import com.github.tototoshi.org.layout._
import org.apache.commons.io.FileUtils

object Main extends Parser with Renderer {

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
          val result = parseFile(c.inputFile).map { render _ }.flatten
          c.mode match {
            case "slide" => {
              FileUtils.writeStringToFile(c.outputFile, SimpleSlideLayout(result).toString)
            }
            case "html" => {
              FileUtils.writeStringToFile(c.outputFile, BootstrapLayout(result).toString)
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

}
