package edu.towson.cis.cosc455buckman3project01

import edu.towson.cis.cosc455buckman3project01.Constants._

import scala.collection.mutable.Stack
class MySemanticAnalyzer {
  var parser: Stack[String] = Compiler.list

  def gittex(): Unit = {
    parser.reverse
    if (parser.top.equals(DOCB)) {
      println("<html>")
      popToken()
      while (!parser.top.equals(DOCE)) {
        if (parser.top.equalsIgnoreCase(VARIABLEDEFINITIONS)) {
          variableDefine()

          if (parser.top.equals(TITLEB)) {
            title()
          }
          if (checkValidText(parser.top)) {
            body()
          }
        }
        else if (parser.top.equals(DOCE)) {
          println("</html>")
        }
      }
    }
  }

  def title(): Unit = {
    if (parser.top.equalsIgnoreCase(TITLEB)) {
      // add to parse tree / stack
      popToken()
      println("<head>")
      while (!(parser.top.equals(BRACKETE))) {
        if (parser.top.equals(Constants.LINKB)) {
          popToken()
        } else if (checkValidText(parser.top)) {
          popToken()
        }
      }
      println("</head>")

    }
  }

  /*
<body> ::= <inner-text> <body>
| <paragraph> <body>
| <newline> <body>
| ε
 */
  def body(): Unit = {
    if (parser.top.equalsIgnoreCase(PARAGRAPHB)) {
      paragraph()
      // add to parse tree / stack
    } else if (parser.top.equals(BOLD)) {
      bold()
    } else if (parser.top.equals(UNORDEREDLIST)) {
      listItem()
    } else if (parser.top.equalsIgnoreCase(IMAGES)) {
      image()
    } else if (parser.top.equalsIgnoreCase(NEWLINE)) {
      popToken()
      println("")
    }
    else if (checkValidText(parser.top)) {
      innertext()
    }
  }

  //<paragraph> ::= PARAB <variable-define> <inner-text> PARAE
  def paragraph(): Unit =
    if (parser.top.equalsIgnoreCase(PARAGRAPHB)) {
      println("<p>")

      while (!parser.top.equalsIgnoreCase(PARAGRAPHE)) {
        // add to parse tree / stack
        if (parser.top.equalsIgnoreCase(VARIABLEDEFINITIONS)) {
          variableDefine()
        } else if (checkValidText(parser.top)) {
          innertext()
        }
      }
      println("</p>")
    }


  //<heading> ::= HEADING REQTEXT | ε
  def heading(): Unit = {
    if (parser.top.equals(HEADING)) {
      popToken()
      println("<h>")
      popToken()
      println("</h>")
    }
  }


  // <variable-define> ::= DEFB REQTEXT EQSIGN REQTEXT BRACKETE <variable-define> | ε
  def variableDefine(): Unit = {
    if (parser.top.equalsIgnoreCase(VARIABLEDEFINITIONS)) {
      popToken()
      println("<\\DEF>")
      if (checkValidText(parser.top)) {
        println(parser.pop())
        if (parser.top.equals(EQSIGN)) {
          popToken()
          println(EQSIGN)
          if (checkValidText(parser.top)) {
            println(parser.pop)
            if (Compiler.currentToken.equals(BRACKETE)) {
              popToken()
              println("</\\DEF>")
              if (Compiler.currentToken.equals(VARIABLEDEFINITIONS)) {
                variableDefine()
              }
            }
          }
        }
      }
    }
  }

  //<variable-use> ::= USEB REQTEXT BRACKETE | ε

    def variableUse(): Unit = {
    if (parser.top.equals(VARIABLEUSAGE)) {
      popToken()
      println("<\\USE>")
      if (checkValidText(parser.top)) {
        popToken()
        println(parser.top)
        if (parser.top.equals(BRACKETE)) {
          popToken()
        }
      }
    }
  }

  //<bold> ::= BOLD TEXT BOLD | ε
  def bold(): Unit = {
    if (parser.top.equals(BOLD)) {
      println("<b>")
      popToken()
      if (checkValidText(parser.top)) {
        println(parser.top)
        popToken()
        if (parser.top.equals(BOLD)) {
          println("</b>")
        }
      }
    }
  }

  //<listitem> ::= LISTITEMB <inner-item> <list-item> | ε
   def listItem(): Unit = {
    if (parser.top.equals(UNORDEREDLIST)) {
      popToken()
      println("<li>")
      popToken()
      print(parser.top)
      println("</li>")
    }
  }

  //<link> ::= LINKB REQTEXT BRACKETE ADDRESSB REQTEXT ADDRESSE | ε
    def link(): Unit = {
    if (parser.top.equals(LINKB)) {
      popToken()
      if (checkValidText(parser.top)) {
        print(parser.top)
        popToken()
        if (parser.top.equals(BRACKETE)) {
          popToken()
          if (parser.top.equals(ADDRESSB)) {
            address()
          }
        }
      }
    }
  }

  //<image> ::= IMAGEB REQTEXT BRACKETE ADDRESSB REQTEXT ADDRESSE | ε
   def image(): Unit = {
    if (parser.top.equalsIgnoreCase(IMAGES)) {
      /*  addToken()
        if (checkValidText(parser.top)) {
          addToken()
          if (parser.top.equals(BRACKETE)) {
            addToken()
            if (parser.top.equals(ADDRESSB)) {
              address()
            }
          }
        }*/
      popToken()
      link()
    }
  }

  //<newline> ::= NEWLINE | ε
   def newline(): Unit = {
    if (parser.top.equals(NEWLINE)) {
      popToken()
      println("<br>")
    } else {
      println("Error" + Compiler.currentToken + " is incorrect")
      System.exit(1)
    }
  }

  /*
   <inner-text> ::= <variable-use> <inner-text>
| <heading> <inner-text>
| <bold> <inner-text>
| <listitem> <inner-text>
| <image> <inner-text>
| <link> <inner-text>
| TEXT <inner-text>
| ε
    */
  def innertext(): Unit = {
    if (parser.top.equalsIgnoreCase(VARIABLEUSAGE)) {
      variableUse()
    } else if (parser.top.equals(HEADING)) {
      heading()
    } else if (parser.top.equals(BOLD)) {
      bold()
    } else if (parser.top.equalsIgnoreCase(UNORDEREDLIST)) {
      listItem()
    } else if (parser.top.equals(IMAGES)) {
      image()
    } else if (parser.top.equals(LINKB)) {
      link()
    } else if (checkValidText(parser.top)) {
      popToken()
    }
  }

  /*
  <inner-item> ::= <variable-use> <inner- item>
| <bold> <inner- item>
| <link> <inner- item>
| REQTEXT <inner- item>
| ε
   */
  def innerItem(): Unit = {
    if (parser.top.equalsIgnoreCase(VARIABLEUSAGE)) {
      variableUse()
    } else if (parser.top.equals(BOLD)) {
      bold()
    } else if (parser.top.equals(LINKB)) {
      link()
    } else if (checkValidText(parser.top)) {
      popToken()
    }
  }

  def address(): Unit = {
    if (parser.top.equals(ADDRESSB)) {
      print("<a href =")
      popToken()
      if (checkValidText(parser.top)) {
        print(parser.top)
        popToken()
        if (parser.top.equals(ADDRESSE)) {
          print(">")
          popToken()
        }
      }
    }
  }

  def popToken(): Unit = {
  parser.pop()

  }

  def checkValidText(candidate:String): Boolean = {
    candidate.toLowerCase.toList.forall(x => Constants.validText.contains(x.toString()))
  }

}

