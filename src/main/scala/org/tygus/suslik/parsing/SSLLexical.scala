package org.tygus.suslik.parsing

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader.EofCh

/**
  * @author Ilya Sergey
  */
class SSLLexical extends StdLexical {

  // Add keywords
  reserved += ("if", "then", "else", "true", "false", "emp", "not", "return", "predicate", "in")
  reserved += ("error","magic","malloc", "free", "let", "assume")

  // Types
  reserved += ("int", "bool", "loc", "set", "void")

  delimiters += ("(", ")", "=", ";", "**", "*", ":->", "=i", "<=i", "++", "--",
      "{", "}", "/\\", "&&", "\\/", "||", "\n", "\r", "=>", "?", ":",
      "<", ">", ",", "/",   "+", "-", "==", "!=", "==>", "<=", ">=", "[", "]", "|", "??"
  )

}
