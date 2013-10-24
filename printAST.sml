use "parser.sml";


fun 
   binOpToStr BOP_PLUS = " + "
 | binOpToStr BOP_MINUS = " - "
 | binOpToStr BOP_TIMES = " * "
 | binOpToStr BOP_DIVIDE = " / "
 | binOpToStr BOP_MOD = " % "
 | binOpToStr BOP_EQ = " == "
 | binOpToStr BOP_NE = " != "
 | binOpToStr BOP_LT = " < "
 | binOpToStr BOP_GT = " > "
 | binOpToStr BOP_LE = " <= "
 | binOpToStr BOP_GE = " >= "
 | binOpToStr BOP_AND = " && "
 | binOpToStr BOP_OR = " || "
 | binOpToStr BOP_COMMA = ", "
;

fun
   unOpToStr UOP_NOT = "!"
 | unOpToStr UOP_TYPEOF = "typeof "
 | unOpToStr UOP_MINUS = "-"
;

val commaPrec = 1;
val condPrec = 2;
val orPrec = 3;
val andPrec = 4;
val eqPrec = 5;
val relPrec = 6;
val addPrec = 7;
val multPrec = 8;
val unaryPrec = 9;

fun 
   binPrec BOP_PLUS = addPrec
 | binPrec BOP_MINUS = addPrec
 | binPrec BOP_TIMES = multPrec
 | binPrec BOP_DIVIDE = multPrec
 | binPrec BOP_MOD = multPrec
 | binPrec BOP_EQ = eqPrec
 | binPrec BOP_NE = eqPrec
 | binPrec BOP_LT = relPrec
 | binPrec BOP_GT = relPrec
 | binPrec BOP_LE = relPrec
 | binPrec BOP_GE = relPrec
 | binPrec BOP_AND = andPrec
 | binPrec BOP_OR = orPrec
 | binPrec BOP_COMMA = commaPrec
;

fun
   unPrec UOP_NOT = unaryPrec
 | unPrec UOP_TYPEOF = unaryPrec
 | unPrec UOP_MINUS = unaryPrec
;

fun printAST (PROGRAM {elems=el}) = 
   print (printProgram el)

and 
   printProgram n = 
      String.concatWith "\n" (map printSourceElement n)

and printSourceElement (STMT {stmt=stmt}) = 
   printStatement stmt

and printStatement (ST_EXP {exp=exp}) = 
   (printExpression exp 0) ^ ";"

and 
   printExpression (EXP_NUM n) _ = Int.toString n
 | printExpression (EXP_STRING n) _ = "\"" ^ n ^ "\""
 | printExpression EXP_TRUE _ = "true"
 | printExpression EXP_FALSE _ = "false"
 | printExpression EXP_UNDEFINED _ = "undefined"
 | printExpression (EXP_BINARY n) prevPrec = printBinary n prevPrec
 | printExpression (EXP_UNARY n) prevPrec = printUnary n prevPrec
 | printExpression (EXP_COND n) prevPrec = printCond n prevPrec 

and printBinary {opr=opr, lft=lft, rht=rht} prevPrec =
   let 
      val prec = binPrec opr
      val ret =  (printExpression lft prec) ^ (binOpToStr opr) 
         ^ (printExpression rht prec)
   in
      (*if prec < prevPrec
      then*)
         "(" ^ ret ^ ")"
      (*else      
         ret*)
   end

and printUnary {opr=opr, opnd=opnd} prevPrec = 
   let 
      val prec = unPrec opr 
      val ret = (unOpToStr opr) ^ (printExpression opnd prec)
   in
      (*if prec < prevPrec
      then*)
         "(" ^ ret ^ ")"
      (*else
         ret*)
   end

and printCond {guard=guard, thenExp=thenExp, elseExp=elseExp} prevPrec = 
   let 
      val prec = condPrec
      val ret = (printExpression guard prec) ^  " ? " ^ (printExpression thenExp prec) 
         ^ " : " ^ (printExpression elseExp prec) 
   in
      (*if prec < prevPrec
      then*)
         "(" ^ ret ^ ")"
      (*else
         ret*)
   end
;
