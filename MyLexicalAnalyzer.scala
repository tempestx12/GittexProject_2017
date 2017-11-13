package edu.towson.cis.cosc455buckman3project01
import scala.collection.mutable.ListBuffer
class MyLexicalAnalyzer extends LexicalAnalyzer {
  private var lexeme = ListBuffer[Char]()
  var nextChar: Char = ' '
  private var lexLength = 0
  private var position = 0
  var sourceLine: String = " "
  var lexmsToken = ListBuffer[String]()


  //Function that starts the lexical Analyzer
  def start(line: String): Unit = {
    initializeLexms()
    sourceLine = line
    position = 0
    getNextToken()
  }

  //Adds input to the lexeme
  override def addChar(): Unit = {
    lexLength += 1
    lexeme += nextChar
  }

  //gets character input and returns next Char
  override def getChar(): Char = {
    if (position < sourceLine.length()) {
      nextChar = sourceLine.charAt(position)
      position += 1
      return nextChar
    } else {
      nextChar = '\\'
      return nextChar
    }

  }

  //gets non blank spaces and new lines until it encounters a Character
  def getNonBlank(): Unit = {
    while (Constants.whiteSpace.contains(nextChar)) {
      addChar()
      getChar()
    }
  }

  //Gets the next Lexical Token
  override def getNextToken(): Unit = {
    lexLength = 0
    //If the current token is a white space or new line cycle until its not
    getNonBlank()
    //Add the first character you get
    //Checks to see if there are any special Symbols and processes them
    addChar()
    getChar()
    if (Constants.symbols.contains(nextChar)) {
      if (nextChar == '\\') {
        checkAnnon()
      } else if (nextChar == '!') {
        checklink()
      } else {
        checkSymbol()
      }
      var newToken: String = lexeme.mkString
      if (lookup(newToken)) {
        Compiler.currentToken = newToken
      }
      //if nextchar isnt a symbol or whitespace
    } else if (nextChar!=(Constants.whiteSpace)) {
      checkText()
      Compiler.currentToken = lexeme.mkString
    } else {
      println("Error cannot get next token")
      System.exit(1)
    }
    lexeme.clear()
  }

  //Adds all the legal lexms to the language using List Buffer
  private def initializeLexms(): Unit = {
    lexmsToken += Constants.DOCB
    lexmsToken += Constants.DOCE
    lexmsToken += Constants.PARAGRAPHB
    lexmsToken += Constants.PARAGRAPHE
    lexmsToken += Constants.IMAGES
    lexmsToken += Constants.UNORDEREDLIST
    lexmsToken += Constants.VARIABLEDEFINITIONS
    lexmsToken += Constants.VARIABLEUSAGE
    lexmsToken += Constants.BOLD
    lexmsToken += Constants.TITLEB
    lexmsToken += Constants.NEWLINE
    lexmsToken += Constants.HEADING
    lexmsToken += Constants.EQSIGN
    lexmsToken += Constants.BRACKETE
    lexmsToken += Constants.ADDRESSE
    lexmsToken += Constants.LINKB
  }


  override def lookup(canidate: String): Boolean = {
    if (lexeme.contains(canidate)) {
      Compiler.Parser.setError()
      println("Lexical Error -" + canidate + "is not a legal token")
      return false
    }
    return true
  }

  def checklink(): Unit = {
    addChar()
    getChar()
  }

  def checkAnnon(): Unit = {
    addChar()
    getChar()
    while ((nextChar != '[') && !(Constants.whiteSpace.contains(nextChar))&& !(Constants.symbols.contains(nextChar))) {
      addChar()
      getChar()
    }
    if(nextChar=="["){
      addChar()
    }
  }

  def checkSymbol(): Unit = {
    addChar()
    getChar()
    if (nextChar == '[') {
      checklink()
    }
  }

  def checkText(): Unit = {
    if (position < sourceLine.length) {
      while ((!Constants.symbols.contains(nextChar))) {
        addChar()
        getChar()
      }
    }
  }
}