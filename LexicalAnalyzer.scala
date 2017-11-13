package edu.towson.cis.cosc455buckman3project01

trait LexicalAnalyzer {
def addChar(): Unit
  def getChar():Char
  def getNextToken(): Unit
  def lookup(candidate: String): Boolean
}
