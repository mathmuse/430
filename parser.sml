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
      parseSourceElement fstr
   end

and parseSourceElement fstr =
   parseStatement fstr

and parseStatement fstr = 
   parseExpressionStatement fstr

and parseExpressionStatement fstr = 
   let val tk1 = parseExpression fstr in
      if 
         tk1 = TK_SEMI
      then 
         ()
      else
         exp ";" tk1
   end

and parseExpression fstr = 
   let val tk1 = parseAssignmentExpression fstr in
      if 
         tk1 = TK_COMMA
      then
         parseExpression fstr
      else
         (tk1)
   end

and parseAssignmentExpression fstr = 
   parseConditionalExpression fstr

and parseConditionalExpression fstr = 
   let val tk1 = (parseLogicalORExpression fstr) in
      if tk1 = TK_QUESTION
      then
         let val tk2 = (parseAssignmentExpression fstr) in
            if tk2 = TK_COLON
            then parseAssignmentExpression fstr 
            else exp ":" tk2 
         end
      else tk1
   end

and parseLogicalORExpression fstr =
   let val tk1 = parseLogicalANDExpression fstr in
      if tk1 = TK_OR
      then parseLogicalORExpression fstr
      else tk1
   end

and parseLogicalANDExpression fstr =
   let val tk1 = parseEqualityExpression fstr in
      if tk1 = TK_AND
      then parseLogicalANDExpression fstr
      else tk1
   end

and parseEqualityExpression fstr = 
   let val tk1 = parseRelationalExpression fstr in
      if isEqOp tk1
      then parseEqualityExpression fstr
      else tk1
   end

and parseRelationalExpression fstr = 
   let val tk1 = parseAdditiveExpression fstr in
      if isRelOp tk1
      then parseRelationalExpression fstr
      else tk1
   end

and parseAdditiveExpression fstr = 
   let val tk1 = parseMultiplicativeExpression fstr in
      if isAddOp tk1
      then parseAdditiveExpression fstr
      else tk1
   end

and parseMultiplicativeExpression fstr = 
   let val tk1 = parseUnaryExpression fstr in
      if isMultOp tk1
      then parseMultiplicativeExpression fstr
      else tk1
   end

and parseUnaryExpression fstr = 
   let val tk1 = nextToken fstr in
      if isUnaryOp tk1
      then 
         let val tk2 = nextToken fstr in 
            parseLeftHandSideExpression fstr tk2
         end
      else parseLeftHandSideExpression fstr tk1
   end

and parseLeftHandSideExpression fstr tk = 
   parseCallExpression fstr tk

and parseCallExpression fstr tk = 
   parseMemberExpression fstr tk

and parseMemberExpression fstr tk = 
   parsePrimaryExpression fstr tk

and parsePrimaryExpression fstr tk =
   let val tk1 = nextToken fstr in  
      case tk of
         TK_LPAREN =>
            let 
               val tk2 = parseExpression fstr
               val tk3 = nextToken fstr
            in
               if tk2 = TK_RPAREN
               then tk3
               else exp ")" tk
            end
       | TK_NUM _ => (tk1) 
       | TK_TRUE => (tk1)
       | TK_FALSE => (tk1)
       | TK_STRING _ => (tk1)
       | TK_UNDEFINED => (tk1)
       | _ => exp "value" tk 
   end
;



