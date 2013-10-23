open TextIO;
use "tokenizer.sml";

fun error s = (output (stdErr, s); OS.Process.exit OS.Process.failure);

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
   if 
      (parseExpression fstr) = TK_SEMI
   then 
      ()
   else
      error "No terminating semicolon!" 

and parseExpression fstr = 
   if 
      (parseAssignmentExpression fstr) = TK_COMMA
   then
      parseExpression fstr
   else
      (TK_COMMA)

and parseAssignmentExpression fstr = 
   parseConditionalExpression fstr

and parseConditionalExpression fstr = 
   let val tk1 = (parseLogicalORExpression fstr) in
      if tk1 = TK_QUESTION
      then
         let val tk2 = (parseAssignmentExpression fstr) in
            if tk2 = TK_COLON
            then parseAssignmentExpression fstr 
            else error "Invalid conditional!"
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
            if (parseExpression fstr) = TK_RPAREN
            then (tk1)
            else error "No closing parentheses"
       | TK_NUM _ => (tk1) 
       | TK_TRUE => (tk1)
       | TK_FALSE => (tk1)
       | TK_STRING _ => (tk1)
       | TK_UNDEFINED => (tk1)
       | _ => error "Unknown terminal!"
   end
;





