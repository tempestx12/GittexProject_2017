package edu.towson.cis.cosc455buckman3project01

import scala.collection.mutable.ListBuffer

object Compiler {
  var currentToken : String = ""
  var fileContents : String = " "
  var listLexeme=ListBuffer[String]()
  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySyntaxAnalyzer

  def main(args: Array[String]): Unit = {
    val file= args(0)
    checkFile(args)
    readFile(args(0))
    Scanner.start(fileContents)
    println(currentToken)
   /*
   while(!currentToken.equals(Constants.DOCE)) {
     listLexeme += currentToken
     Scanner.getNextToken()
     println(currentToken)
   }
*/
    Parser.gittex(fileContents)
System.exit(1)
  }

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
   //println(fileContents)
  }

  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of args fool!")
      System.exit(1)
    }
    else if (! args(0).endsWith(".gtx")) {
      println("USAGE ERROR: wrong extension fool!")
      System.exit(1)
    }
  }
}
