package edu.towson.cis.cosc455buckman3project01

import edu.towson.cis.cosc455buckman3project01.Constants._

import scala.collection.mutable

class MySyntaxAnalyzer extends SyntaxAnalyzer {
  var parser: mutable.Stack[String] = mutable.Stack[String]()
  var errorFound: Boolean = false

  def setError() = errorFound = true

  def resetError() = errorFound = false

  def getError: Boolean = errorFound

  //<gittex> ::= DOCB <variable-define> <title> <body> DOCE
  override def gittex(token: String): Unit = {
    resetError()
    parser.push(Compiler.currentToken)
    if (parser.top.equals(DOCB)) {
      addToken()
      println(Compiler.currentToken)
      if (parser.top.equals(VARIABLEDEFINITIONS)) {
        variableDefine()
      }
      if (parser.top.equals(TITLEB)) {
        title()
      }
      if (checkValidText(parser.top)) {
        body()
      }
      if (Compiler.currentToken.equalsIgnoreCase(DOCE)) {
        addToken()
        println("noice")
      }

    }
    else {
      println("Error " + Compiler.currentToken + " is incorrect")
      setError()
      System.exit(1)
    }
  }

  //<title> ::= TITLEB REQTEXT BRACKETE
  override def title(): Unit = {
    if (parser.top.equals(TITLEB)) {
      // add to parse tree / stack
      addToken()
      while ((!Compiler.currentToken.equals(BRACKETE)) && (!errorFound)) {
        if (parser.top.equals(Constants.TitleS)) {
          addToken()
        } else if (checkValidText(parser.top)) {
          addToken()
        }
        /*else {
          setError()
          println("Error" + parser.top + " is incorrect")
        }
        */
      }
    }
  }

  /*
<body> ::= <inner-text> <body>
| <paragraph> <body>
| <newline> <body>
| ε
 */
  override def body(): Unit = {
    if (parser.top.equals(PARAGRAPHB)) {
      paragraph()
      // add to parse tree / stack
    } else if (parser.top.equals(BOLD)) {
      bold()
    } else if (parser.top.equals(UNORDEREDLIST)) {
      listItem()
    } else if (parser.top.equalsIgnoreCase(IMAGES)) {
      image()
    }
    else {
      println("Error")
      System.exit(1)
    }
  }

  //<paragraph> ::= PARAB <variable-define> <inner-text> PARAE
  override def paragraph(): Unit =
    if (parser.top.equalsIgnoreCase(PARAGRAPHB)) {
      addToken()
      while (!parser.top.equalsIgnoreCase(PARAGRAPHE)) {
        // add to parse tree / stack
        if (parser.top.equalsIgnoreCase(VARIABLEDEFINITIONS)) {
          variableDefine()
        } else if (checkValidText(parser.top)) {
          innertext()
        }
      }
    }
    else {
      println("Error" + parser.top + " is incorrect")
      setError()
      System.exit(1)
    }

  //<heading> ::= HEADING REQTEXT | ε
  override def heading(): Unit = {
    if (parser.top.equals(HEADING)) {
      addToken()
    }
    else {
      println("Error" + Compiler.currentToken + " is incorrect")
      System.exit(1)
    }
  }

  // <variable-define> ::= DEFB REQTEXT EQSIGN REQTEXT BRACKETE <variable-define> | ε
  override def variableDefine(): Unit = {
    if (parser.top.equalsIgnoreCase(VARIABLEDEFINITIONS)) {
      addToken()

      if (checkValidText(parser.top)) {
        addToken()
        if (parser.top.equals(EQSIGN)) {
          addToken()
          if (checkValidText(parser.top)) {
            addToken()
            if (Compiler.currentToken.equals(BRACKETE)) {
              addToken()
              if (Compiler.currentToken.equals(VARIABLEDEFINITIONS)) {
                variableDefine()
              }
            } else {
              setError()
              println("Error" + Compiler.currentToken + " is incorrect")
              System.exit(1)
            }
          } else {
            setError()
            println("Error" + Compiler.currentToken + " is incorrect")
            System.exit(1)
          }
        } else {
          setError()
          println("Error" + Compiler.currentToken + " is incorrect")
          System.exit(1)
        }
      } else {
        setError()
        println("Error" + Compiler.currentToken + " is incorrect")
        System.exit(1)

      }
    }
    else {
      setError()
      println("Error" + Compiler.currentToken + " is incorrect")
      System.exit(1)
    }
  }

  //<variable-use> ::= USEB REQTEXT BRACKETE | ε

  override def variableUse(): Unit = {
    if (parser.top.equals(VARIABLEUSAGE)) {
      addToken()
      if (checkValidText(parser.top)) {
        addToken()
        if (parser.top.equals(BRACKETE)) {
          addToken()
        } else {
          setError()
          println("Error" + parser.top + "is incorrect")
          System.exit(1)
        }
      } else {
        setError()
        println("Error" + Compiler.currentToken + "is incorrect")
        System.exit(1)
      }
    } else {
      setError()
      println("Error" + Compiler.currentToken + " is incorrect")
      System.exit(1)
    }
  }

  //<bold> ::= BOLD TEXT BOLD | ε
  override def bold(): Unit = {
    if (parser.top.equals(BOLD)) {
      addToken()
      if (checkValidText(parser.top)) {
        addToken()
        if (parser.top.equals(BOLD)) {
          addToken()
        }
        else {
          setError()
          println("Error" + Compiler.currentToken + "is incorrect")
          System.exit(1)
        }
      } else {
        setError()
        println("Error" + Compiler.currentToken + "is incorrect")
        System.exit(1)
      }
    }
    else {
      setError()
      println("Error" + Compiler.currentToken + " is incorrect")
      System.exit(1)
    }
  }

  //<listitem> ::= LISTITEMB <inner-item> <list-item> | ε
  override def listItem(): Unit = {
    if (parser.top.equals(UNORDEREDLIST)) {
      addToken()
      innerItem()
    } else {
      println("Error" + Compiler.currentToken + " is incorrect")
      System.exit(1)
    }
  }

  //<link> ::= LINKB REQTEXT BRACKETE ADDRESSB REQTEXT ADDRESSE | ε
  override def link(): Unit = {
    if (parser.top.equals(LINKB)) {
      addToken()
      if (checkValidText(parser.top)) {
        addToken()
        if (parser.top.equals(BRACKETE)) {
          addToken()
          if (parser.top.equals(ADDRESSB)) {
            address()
          } else {
            setError()
            println("Error" + parser.top + " is incorrect")
            System.exit(1)
          }
        } else {
          setError()
          println("Error" + Compiler.currentToken + " is incorrect")
          System.exit(1)
        }
      } else {
        setError()
        println("Error" + Compiler.currentToken + " is incorrect")
        System.exit(1)
      }
    } else {
      setError()
      println("Error" + Compiler.currentToken + " is incorrect")
      System.exit(1)
    }
  }

  //<image> ::= IMAGEB REQTEXT BRACKETE ADDRESSB REQTEXT ADDRESSE | ε
  override def image(): Unit = {
    if (parser.top.equalsIgnoreCase(IMAGES)) {
      addToken()
      if (checkValidText(parser.top)) {
        addToken()
        if (parser.top.equals(BRACKETE)) {
          addToken()
          if (parser.top.equals(ADDRESSB)) {
            address()
          }
        }
      }

    } else {
      setError()
      println("Error" + Compiler.currentToken + " is incorrect")
      System.exit(1)
    }
  }

  //<newline> ::= NEWLINE | ε
  override def newline(): Unit = {
    if (parser.top.equals(NEWLINE)) {
      addToken()
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
      addToken()
    } else {
      setError()
      println("Error" + Compiler.currentToken + " is incorrect")
      System.exit(1)
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
      addToken()
    }  else {
      setError()
      println("Error" + Compiler.currentToken + " is incorrect")
      System.exit(1)
    }
  }

  def address(): Unit = {
    if (parser.top.equals(ADDRESSB)) {
      addToken()
      if (checkValidText(parser.top)) {
        addToken()
        if (parser.top.equals(ADDRESSE)) {
          addToken()
        } else {
          setError()
          println("Error" + Compiler.currentToken + "is incorrect")
          System.exit(1)
        }
      } else {
        setError()
        println("Error" + Compiler.currentToken + "is incorrect")
        System.exit(1)
      }
    } else {
      setError()
      println("Error" + Compiler.currentToken + "is incorrect")
      System.exit(1)
    }

  }

  def addToken(): Unit = {

    Compiler.Scanner.getNextToken()
    println(Compiler.currentToken)
    parser.push(Compiler.currentToken)
  }

  def checkValidText(candidate:String): Boolean = {
    candidate.toLowerCase.toList.forall(x => Constants.validText.contains(x))
  }
  }

