package edu.towson.cis.cosc455buckman3project01

import scala.collection.mutable.ListBuffer

class MySyntaxAnalyzer extends SyntaxAnalyzer{
   var parser =ListBuffer[String]()
  var errorFound: Boolean = false
  def setError() = errorFound = true

  def resetError() = errorFound = false

  def getError: Boolean = errorFound
//<gittex> ::= DOCB <variable-define> <title> <body> DOCE
  override def gittex(token:String): Unit = {
    resetError()
    parser+=Compiler.currentToken
    Compiler.currentToken="\\BEGIN\n\t"
    if (Compiler.currentToken.equalsIgnoreCase("\\BEGIN\n\t")){
          addToken()
      println(Compiler.currentToken)
      if(Compiler.currentToken.equals(Constants.VARIABLEDEFINITIONS)){
        variableDefine()
      }
      else if(Compiler.currentToken.equalsIgnoreCase(Constants.TITLEB)){
        title()
      }else if(Compiler.currentToken.equals(Constants.validText)){
        body()
      }
      if(Compiler.currentToken.equalsIgnoreCase(Constants.DOCE)){
      addToken()
        println("noice")
      }

    }
    else {
      println("Error "+Compiler.currentToken+" is incorrect")
      setError()
      System.exit(1)
    }
  }

//<title> ::= TITLEB REQTEXT BRACKETE
  override def title(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(Constants.TITLEB)){
      // add to parse tree / stack
     addToken()
      while(!Compiler.currentToken.equals(Constants.BRACKETE)){
        if(Compiler.currentToken.equals(Constants.plainText)){
          addToken()
        }else{
          setError()
          println("Error"+Compiler.currentToken+" is incorrect")
        }
      }
      addToken()
    }
    else {
      println("Error"+Compiler.currentToken+" is incorrect")
      System.exit(1)
    }
  }

/*
<body> ::= <inner-text> <body>
| <paragraph> <body>
| <newline> <body>
| ε
 */
  override def body(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(Constants.PARAGRAPHB)){
      paragraph()
      // add to parse tree / stack
      Compiler.Scanner.getNextToken()
    }else if(Compiler.currentToken.equals(Constants.BOLD)){
      bold()
      Compiler.Scanner.getNextToken()
    } else if(Compiler.currentToken.equals(Constants.UNORDEREDLIST)){
      listItem()
      Compiler.Scanner.getNextToken()
    }else if(Compiler.currentToken.equalsIgnoreCase(Constants.IMAGES)){
      image()
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Error")
      System.exit(1)
    }
  }
//<paragraph> ::= PARAB <variable-define> <inner-text> PARAE
  override def paragraph(): Unit =
    if (Compiler.currentToken.equalsIgnoreCase(Constants.PARAGRAPHB)) {
      addToken()
      while (!Compiler.currentToken.equalsIgnoreCase(Constants.PARAGRAPHE)) {
        // add to parse tree / stack
        if(Compiler.currentToken.equalsIgnoreCase(Constants.VARIABLEDEFINITIONS)){
        variableDefine()
        }else if(Compiler.currentToken.equals(Constants.plainText)) {
          innertext()
        }
      }
    }
  else {
    println("Error"+Compiler.currentToken+" is incorrect")
    setError()
    System.exit(1)
  }
//<heading> ::= HEADING REQTEXT | ε
  override def heading(): Unit = {
    if(Compiler.currentToken.equals(Constants.HEADING)){
      addToken()
    }else if(Compiler.currentToken.equals(Nil)){
      addToken()
    }
    else{
      println("Error"+Compiler.currentToken+" is incorrect")
      System.exit(1)
    }
  }
 // <variable-define> ::= DEFB REQTEXT EQSIGN REQTEXT BRACKETE <variable-define> | ε
  override def variableDefine(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(Constants.VARIABLEDEFINITIONS)){
      addToken()

      if(Compiler.currentToken.equals(Constants.plainText)) {
        addToken()
        if (Compiler.currentToken.endsWith(Constants.EQSIGN)) {

          addToken()
          if(Compiler.currentToken.equals(Constants.plainText)) {
            addToken()
            if(Compiler.currentToken.equals(Constants.BRACKETE)){
              addToken()
              if(Compiler.currentToken.equals(Constants.VARIABLEDEFINITIONS)){
                variableDefine()
              }
            }else{
              setError()
              println("Error"+Compiler.currentToken+" is incorrect")
              System.exit(1)
            }
          }else{
            setError()
            println("Error"+Compiler.currentToken+" is incorrect")
            System.exit(1)
          }
        }else{
          setError()
          println("Error"+Compiler.currentToken+" is incorrect")
          System.exit(1)
        }
      }else{
        setError()
        println("Error"+Compiler.currentToken+" is incorrect")
        System.exit(1)

      }
    }else if(Compiler.currentToken.equals(Nil)){
      addToken()
    }
    else
    {
      setError()
      println("Error"+Compiler.currentToken+" is incorrect")
      System.exit(1)
    }
  }
//<variable-use> ::= USEB REQTEXT BRACKETE | ε

  override def variableUse(): Unit = {
    if(Compiler.currentToken.equals(Constants.VARIABLEUSAGE)){
      addToken()
      if(Compiler.currentToken.equals(Constants.plainText)){
        addToken()
        if(Compiler.currentToken.equals(Constants.BRACKETE)){
          addToken()
        }else{
          setError()
          println("Error"+Compiler.currentToken+"is incorrect")
          System.exit(1)
        }
      }else{
        setError()
        println("Error"+Compiler.currentToken+"is incorrect")
        System.exit(1)
      }
    }else
    {
      setError()
      println("Error"+Compiler.currentToken+" is incorrect")
      System.exit(1)
    }
  }
//<bold> ::= BOLD TEXT BOLD | ε
  override def bold(): Unit = {
    if(Compiler.currentToken.equals(Constants.BOLD)){
 addToken()
      if(Compiler.currentToken.equals(Constants.validText)){
        addToken()
        if(Compiler.currentToken.equals((Constants.BOLD))){
          addToken()
        }
        else{
          setError()
          println("Error"+Compiler.currentToken+"is incorrect")
          System.exit(1)
        }
      }else{
        setError()
        println("Error"+Compiler.currentToken+"is incorrect")
        System.exit(1)
      }
    }else if(Compiler.currentToken.equals(Nil)){
      addToken()
    }
    else
      {
        setError()
        println("Error"+Compiler.currentToken+" is incorrect")
        System.exit(1)
      }
  }
  //<listitem> ::= LISTITEMB <inner-item> <list-item> | ε
  override def listItem(): Unit = {
    if(Compiler.currentToken.equals(Constants.UNORDEREDLIST)){
        addToken()
        innerItem()
    }else
    {
      println("Error"+Compiler.currentToken+" is incorrect")
      System.exit(1)
    }
  }
//<link> ::= LINKB REQTEXT BRACKETE ADDRESSB REQTEXT ADDRESSE | ε
  override def link(): Unit = {
    if(Compiler.currentToken.equals(Constants.LINKB)){
  addToken()
      if(Compiler.currentToken.equals(Constants.plainText)){
        addToken()
        if(Compiler.currentToken.equals(Constants.BRACKETE)){
          addToken()
          if(Compiler.currentToken.equals(Constants.ADDRESSB)){
            address()
          }else{
            setError()
            println("Error"+Compiler.currentToken+" is incorrect")
            System.exit(1)
          }
        }else{
          setError()
          println("Error"+Compiler.currentToken+" is incorrect")
          System.exit(1)
        }
      }else{
        setError()
        println("Error"+Compiler.currentToken+" is incorrect")
        System.exit(1)
      }
    }else
    {
      setError()
      println("Error"+Compiler.currentToken+" is incorrect")
      System.exit(1)
    }
  }
//<image> ::= IMAGEB REQTEXT BRACKETE ADDRESSB REQTEXT ADDRESSE | ε
  override def image(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(Constants.IMAGES)) {
      addToken()
      if (Compiler.currentToken.equals(Constants.plainText)) {
        addToken()
        if (Compiler.currentToken.equals(Constants.BRACKETE)) {
          addToken()
          if (Compiler.currentToken.equals(Constants.ADDRESSB)) {
            address()
          }
        }
      }

    } else if(Compiler.currentToken.equals(Nil)){
      addToken()
    }else{
      setError()
      println("Error"+Compiler.currentToken+" is incorrect")
      System.exit(1)
    }
  }
//<newline> ::= NEWLINE | ε
  override def newline(): Unit = {
    if(Compiler.currentToken.equals(Constants.NEWLINE)){
      addToken()
    }else {
      println("Error"+Compiler.currentToken+" is incorrect")
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
   def innertext():Unit={
     if(Compiler.currentToken.equalsIgnoreCase(Constants.VARIABLEUSAGE)){
       variableUse()
     }else if(Compiler.currentToken.equals(Constants.HEADING)){
       heading()
     }else if(Compiler.currentToken.equals(Constants.BOLD)){
       bold()
     }else if(Compiler.currentToken.equalsIgnoreCase(Constants.UNORDEREDLIST)){
       listItem()
     }else if(Compiler.currentToken.equals(Constants.IMAGES)){
       image()
     }else if(Compiler.currentToken.equals(Constants.LINKB)){
       link()
     }else if(Compiler.currentToken.equals(Constants.validText)){
       addToken()
     }else{
       setError()
       println("Error"+Compiler.currentToken+" is incorrect")
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
   def innerItem():Unit={
     if(Compiler.currentToken.equalsIgnoreCase(Constants.VARIABLEUSAGE)){
       variableUse()
     }else if(Compiler.currentToken.equals(Constants.BOLD)){
       bold()
     }else if(Compiler.currentToken.equals(Constants.LINKB)){
       link()
     }else if(Compiler.currentToken.equals(Constants.plainText)){
     addToken()
     }else if(Compiler.currentToken.equals(Nil)){
      addToken()
     }else{
       setError()
       println("Error"+Compiler.currentToken+" is incorrect")
       System.exit(1)
     }
   }
  def address(): Unit ={
    if(Compiler.currentToken.equals(Constants.ADDRESSB)){
      addToken()
      if(Compiler.currentToken.equals(Constants.plainText)){
        addToken()
        if(Compiler.currentToken.equals(Constants.ADDRESSE)){
          addToken()
        }else{
          setError()
          println("Error"+Compiler.currentToken+"is incorrect")
          System.exit(1)
        }
      }else{
        setError()
        println("Error"+Compiler.currentToken+"is incorrect")
        System.exit(1)
      }
    }else{
      setError()
      println("Error"+Compiler.currentToken+"is incorrect")
      System.exit(1)
    }

  }
  def addToken(): Unit ={
    Compiler.Scanner.getNextToken()
    parser+=Compiler.currentToken
  }


}

