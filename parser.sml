open TextIO;
use "tokenizer.sml";
use "ast.sml";

fun error s = (output (stdErr, s); OS.Process.exit OS.Process.failure);

val tokenMap = [
   (TK_LBRACE, "{"),
   (TK_RBRACE, "}"),
   (TK_LPAREN, "("),
   (TK_RPAREN, ")"),
   (TK_LBRACKET, "["),
   (TK_RBRACKET, "]"),
   (TK_COMMA, ","),
   (TK_SEMI, ";"),
   (TK_QUESTION, "?"),
   (TK_COLON, ":"),
   (TK_DOT, "."),
   (TK_PLUS, "+"),
   (TK_MINUS, "-"),
   (TK_TIMES, "*"),
   (TK_DIVIDE, "/"),
   (TK_MOD, "%"),
   (TK_AND, "&&"),
   (TK_OR, "||"),
   (TK_ASSIGN, "="),
   (TK_EQ, "=="),
   (TK_LT, "<"),
   (TK_LE, "<="),
   (TK_GT, ">"),
   (TK_GE, ">="),
   (TK_NOT, "!"),
   (TK_NE, "!="),
   (TK_ELSE, "else"),
   (TK_FALSE, "false"),
   (TK_FUNCTION, "function"),
   (TK_IF, "if"),
   (TK_NEW, "new"),
   (TK_PRINT, "print"),
   (TK_RETURN, "return"),
   (TK_THIS, "this"),
   (TK_TRUE, "true"),
   (TK_TYPEOF, "typeof"),
   (TK_UNDEFINED, "undefined"),
   (TK_VAR, "var"),
   (TK_WHILE, "while"),
   (TK_EOF, "eof")
];


fun 
   tkToStr (TK_NUM n) = Int.toString(n)
 | tkToStr (TK_ID n) = n
 | tkToStr (TK_STRING n) = n
 | tkToStr n = case pairLookup n tokenMap of
      SOME n => n
    | NONE => error "AHHHH!!!!"
; 

fun exp a b = 
   error ("expected '" ^ a ^ "', found '" ^ (tkToStr b) ^ "'\n")
;

fun
   isExpression TK_NOT = true
 | isExpression TK_TYPEOF = true
 | isExpression TK_MINUS = true
 | isExpression TK_LPAREN = true
 | isExpression (TK_NUM _) = true
 | isExpression TK_TRUE = true
 | isExpression TK_FALSE = true
 | isExpression (TK_STRING _) = true
 | isExpression TK_UNDEFINED = true
 | isExpression _ = false
;

fun isEqOp tk = 
   tk = TK_EQ orelse 
   tk = TK_NE
;

fun isRelOp tk = 
   tk = TK_LT orelse
   tk = TK_LE orelse
   tk = TK_GT orelse
   tk = TK_GE
;

fun isAddOp tk = 
   tk = TK_PLUS orelse
   tk = TK_MINUS
;

fun isMultOp tk =
   tk = TK_TIMES orelse
   tk = TK_DIVIDE orelse
   tk = TK_MOD
;

fun isUnaryOp tk = 
   tk = TK_NOT orelse
   tk = TK_TYPEOF orelse
   tk = TK_MINUS
;

fun isAndOp tk = 
   tk = TK_AND
;

fun isOrOp tk = 
   tk = TK_OR
;

fun isCommaOp tk = 
   tk = TK_COMMA
;

fun 
   unTkToOp TK_NOT = UOP_NOT
 | unTkToOp TK_TYPEOF = UOP_TYPEOF
 | unTkToOp TK_MINUS = UOP_MINUS
 | unTkToOp n = error ":("
;

fun 
   binTkToOp TK_PLUS = BOP_PLUS
 | binTkToOp TK_TIMES = BOP_MINUS
 | binTkToOp TK_DIVIDE = BOP_DIVIDE
 | binTkToOp TK_MOD = BOP_MOD
 | binTkToOp TK_EQ = BOP_EQ
 | binTkToOp TK_NE = BOP_NE
 | binTkToOp TK_LT = BOP_LT
 | binTkToOp TK_GT = BOP_GT
 | binTkToOp TK_LE = BOP_LE
 | binTkToOp TK_GE = BOP_GE
 | binTkToOp TK_AND = BOP_AND
 | binTkToOp TK_OR = BOP_OR
 | binTkToOp TK_COMMA = BOP_COMMA
 | binTkToOp n = error ":(("
;


fun parseBinary nextFun tkChk fstr tk =  
   let val (tk1, ast) = nextFun fstr tk in 
      if tkChk tk1
      then parseRecBin nextFun tkChk (binTkToOp tk1) fstr (nextToken fstr) ast
      else (tk1, ast)
   end

and parseRecBin nextFun tkChk opr fstr tk lft = 
   let val (tk1, ast) = nextFun fstr tk
       val newast = EXP_BINARY {opr=opr, lft=lft, rht=ast}
   in 
      if tkChk tk1
      then parseRecBin nextFun tkChk (binTkToOp tk1) fstr (nextToken fstr) newast
      else (tk1, newast)
   end
;

fun parse fname =
   let 
      val fstr = TextIO.openIn fname;
      val ast = parseProgram fstr (nextToken fstr)
   in
      PROGRAM {elems = ast}
   end

and parseProgram fstr tk = 
   if isExpression tk
   then 
      let
         val (tk1, ast) = parseSourceElement fstr tk
      in
         ast :: (parseProgram fstr tk1)
      end
   else
      []

and parseSourceElement fstr tk =
   parseStatement fstr tk

and parseStatement fstr tk = 
   let val (tk1, ast) = parseExpressionStatement fstr tk
   in
      (tk1, STMT {stmt=ast})
   end

and parseExpressionStatement fstr tk = 
   let val (tk1, ast1) = parseExpression fstr tk
   in
      if tk1 = TK_SEMI
      then
         (nextToken fstr, ST_EXP {exp=ast1})
      else
         exp ";" tk1
   end


and parseExpression fstr tk = 
   parseBinary parseAssignmentExpression isCommaOp fstr tk

and parseAssignmentExpression fstr tk = 
   parseConditionalExpression fstr tk

and parseConditionalExpression fstr tk = 
   let val (tk1, ast1) = (parseLogicalORExpression fstr tk) in
      if tk1 = TK_QUESTION
      then
         let val (tk2, ast2) = (parseAssignmentExpression fstr (nextToken fstr)) in
            if tk2 = TK_COLON
            then 
               let val (tk3, ast3) = parseAssignmentExpression fstr (nextToken fstr)
               in
                  (tk3, EXP_COND {guard=ast1, thenExp=ast2, elseExp=ast3})
               end
            else exp ":" tk2 
         end
      else (tk1, ast1)
   end

and parseLogicalORExpression fstr tk =
   parseBinary parseLogicalANDExpression isOrOp fstr tk

and parseLogicalANDExpression fstr tk = 
   parseBinary parseEqualityExpression isAndOp fstr tk

and parseEqualityExpression fstr tk = 
   parseBinary parseRelationalExpression isEqOp fstr tk

and parseRelationalExpression fstr tk = 
   parseBinary parseAdditiveExpression isRelOp fstr tk

and parseAdditiveExpression fstr tk = 
   parseBinary parseMultiplicativeExpression isAddOp fstr tk

and parseMultiplicativeExpression fstr tk = 
   parseBinary parseUnaryExpression isMultOp fstr tk

and parseUnaryExpression fstr tk = 
   if isUnaryOp tk
   then 
      let val tk1 = nextToken fstr
          val (tk2, ast) = parseLeftHandSideExpression fstr tk1
      in
         (tk2, EXP_UNARY {opr = (unTkToOp tk), opnd = ast})
      end
   else parseLeftHandSideExpression fstr tk

and parseLeftHandSideExpression fstr tk = 
   parseCallExpression fstr tk

and parseCallExpression fstr tk = 
   parseMemberExpression fstr tk

and parseMemberExpression fstr tk = 
   parsePrimaryExpression fstr tk

and parsePrimaryExpression fstr tk =
   case tk of
      TK_LPAREN =>
         let 
            val (tk2, ast) = parseExpression fstr (nextToken fstr)
         in
            if tk2 = TK_RPAREN
            then (nextToken fstr, ast)
            else exp ")" tk
         end
    | TK_NUM n => (nextToken fstr, EXP_NUM n)
    | TK_TRUE => (nextToken fstr, EXP_TRUE)
    | TK_FALSE => (nextToken fstr, EXP_FALSE)
    | TK_STRING n => (nextToken fstr, EXP_STRING n)
    | TK_UNDEFINED => (nextToken fstr, EXP_UNDEFINED)
    | _ => exp "value" tk

;



