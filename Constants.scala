package edu.towson.cis.cosc455buckman3project01

object Constants {
  val DOCB: String = "\\BEGIN"
  val DOCE: String = "\\END"
  val TITLEB: String = "\\TITLE"
  val BRACKETE: String = "]"
  val HEADING: String = "#"
  val LINKB: String ="["
  val PARAGRAPHB: String = "\\PARAB"
  val PARAGRAPHE: String = "\\PARAE"
  val BOLD: String = "*"
  val UNORDEREDLIST: String = "+"
  val NEWLINE: String = "\\\\"
  val IMAGES: String = "!"
  val ADDRESSB: String ="("
  val ADDRESSE: String = ")"
  val EQSIGN: String = "="
  val VARIABLEDEFINITIONS: String = "\\DEF["
  val VARIABLEUSAGE: String = "\\USE["
  val symbols:List[Char]=List('#','!','[','(','\\','+','=','*',']',')')
  val letters: List[String] = List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
  val numbersEtc: List[String] = List("1", "2", "3", "4", "5", "6", "7", "8", "9", "0", ",", ".", "\"", ":", "?", "_", "/", "'", "" , "]","#","!","\\","[","=","(",")","+","*")
  val whiteSpace: List[String] = List(" ", "\t", "\n", "\b", "\f", "\r")
  val emptySpace: List[Char]= List(' ', '\t', '\n', '\b','\f', '\r')
  val validText: List[String] = whiteSpace ::: letters ::: numbersEtc
  val plainText: List[String]=letters ::: numbersEtc
}