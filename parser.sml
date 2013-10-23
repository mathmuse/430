open TextIO;
use "tokenizer.sml";

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

fun parse fname =
   let 
      val fstr = TextIO.openIn fname;
   in
      parseSourceElement fstr (nextToken fstr)
   end

and parseSourceElement fstr tk =
   parseStatement fstr tk

and parseStatement fstr tk = 
   parseExpressionStatement fstr

and parseExpressionStatement fstr tk = 
   if isExpression tk
   then 
      let val tk1 = parseExpression fstr tk in
         if 
            tk1 = TK_SEMI
         then 
            ()
         else
            exp ";" tk1
      end
   else if tk=TK_EOF then
      ()
   else 
      exp "eof" tk

and parseExpression fstr tk = 
   let val tk1 = parseAssignmentExpression fstr tk in
      if 
         tk1 = TK_COMMA
      then
         parseExpression fstr (nextToken fstr)
      else
         (tk1)
   end

and parseAssignmentExpression fstr tk = 
   parseConditionalExpression fstr tk

and parseConditionalExpression fstr tk = 
   let val tk1 = (parseLogicalORExpression fstr tk) in
      if tk1 = TK_QUESTION
      then
         let val tk2 = (parseAssignmentExpression fstr (nextToken fstr)) in
            if tk2 = TK_COLON
            then parseAssignmentExpression fstr (nextToken fstr)
            else exp ":" tk2 
         end
      else tk1
   end

and parseLogicalORExpression fstr tk =
   let val tk1 = parseLogicalANDExpression fstr tk in
      if tk1 = TK_OR
      then parseLogicalORExpression fstr (nextToken fstr)
      else tk1
   end

and parseLogicalANDExpression fstr tk =
   let val tk1 = parseEqualityExpression fstr tk in
      if tk1 = TK_AND
      then parseLogicalANDExpression fstr (nextToken fstr)
      else tk1
   end

and parseEqualityExpression fstr tk = 
   let val tk1 = parseRelationalExpression fstr tk in
      if isEqOp tk1
      then parseEqualityExpression fstr (nextToken fstr)
      else tk1
   end

and parseRelationalExpression fstr tk = 
   let val tk1 = parseAdditiveExpression fstr tk in
      if isRelOp tk1
      then parseRelationalExpression fstr (nextToken fstr)
      else tk1
   end

and parseAdditiveExpression fstr tk = 
   let val tk1 = parseMultiplicativeExpression fstr tk in
      if isAddOp tk1
      then parseAdditiveExpression fstr (nextToken fstr)
      else tk1
   end

and parseMultiplicativeExpression fstr tk = 
   let val tk1 = parseUnaryExpression fstr tk in
      if isMultOp tk1
      then parseMultiplicativeExpression fstr (nextToken fstr)
      else tk1
   end

and parseUnaryExpression fstr tk = 
   if isUnaryOp tk
   then 
      let val tk1 = nextToken fstr in 
         parseLeftHandSideExpression fstr tk1
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
            val tk2 = parseExpression fstr (nextToken fstr)
         in
            if tk2 = TK_RPAREN
            then nextToken fstr
            else exp ")" tk
         end
    | TK_NUM _ => nextToken fstr
    | TK_TRUE => nextToken fstr
    | TK_FALSE => nextToken fstr
    | TK_STRING _ => nextToken fstr
    | TK_UNDEFINED => nextToken fstr
    | _ => exp "value" tk

;



