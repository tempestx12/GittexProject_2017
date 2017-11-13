package edu.towson.cis.cosc455buckman3project01

  trait SyntaxAnalyzer {
    def gittex(token:String) : Unit
    def title() : Unit
    def body() : Unit
    def paragraph() : Unit
    def heading() : Unit
    def variableDefine() : Unit
    def variableUse() : Unit
    def bold() : Unit
    def listItem() : Unit
    def link() : Unit
    def image() : Unit
    def newline() : Unit
}
