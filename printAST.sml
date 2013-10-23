use "parser.sml";


fun 
   binOpToStr BOP_PLUS = "+"
 | binOpToStr BOP_MINUS = "-"
 | binOpToStr BOP_TIMES = "*"
 | binOpToStr BOP_DIVIDE = "/"
 | binOpToStr BOP_MOD = "%"
 | binOpToStr BOP_EQ = "=="
 | binOpToStr BOP_NE = "!="
 | binOpToStr BOP_LT = "<"
 | binOpToStr BOP_GT = ">"
 | binOpToStr BOP_LE = "<="
 | binOpToStr BOP_GE = ">="
 | binOpToStr BOP_AND = "&&"
 | binOpToStr BOP_OR = "||"
 | binOpToStr BOP_COMMA = ","
;

fun
   unOpToStr UOP_NOT = "!"
 | unOpToStr UOP_TYPEOF = "typeof"
 | unOpToStr UOP_MINUS = "-"
;

fun printAST (PROGRAM {elems=el}) = 
   String.fromString (printProgram el)

and 
   printProgram (h::t) = 
      (printSourceElement h) ^ "\n" ^ (printProgram t)
 | printProgram [] = 
      ""

and printSourceElement (STMT {stmt=stmt}) = 
   printStatement stmt 

and printStatement (ST_EXP {exp=exp}) = 
   printExpression exp

and 
   printExpression (EXP_NUM n) = Int.toString 5
 | printExpression (EXP_STRING n) = "\"" ^ n ^ "\""
 | printExpression EXP_TRUE = "true"
 | printExpression EXP_FALSE = "false"
 | printExpression EXP_UNDEFINED = "undefined"
 | printExpression (EXP_BINARY n) = printBinary n
 | printExpression (EXP_UNARY n) = printUnary n
 | printExpression (EXP_COND n) = printCond n

and printBinary {opr=opr, lft=lft, rht=rht} = 
   "(" ^ (printExpression lft) ^ (binOpToStr opr) ^ (printExpression rht) ^ ")"

and printUnary {opr=opr, opnd=opnd} = 
   (unOpToStr opr) ^ "(" ^ (printExpression opnd) ^ ")"

and printCond {guard=guard, thenExp=thenExp, elseExp=elseExp} = 
   (printExpression guard) ^  "?" ^ (printExpression thenExp) 
   ^ ":" ^ (printExpression elseExp) 

;
