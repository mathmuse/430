use "printAST.sml";

fun 
   getBinString BOP_PLUS = "+"
 | getBinString BOP_MINUS = "-"
 | getBinString BOP_TIMES = "*"
 | getBinString BOP_DIVIDE = "/"
 | getBinString BOP_MOD = "%"
 | getBinString BOP_EQ = "=="
 | getBinString BOP_NE = "!="
 | getBinString BOP_LT = "<"
 | getBinString BOP_GT = ">"
 | getBinString BOP_LE = "<="
 | getBinString BOP_GE = ">="
 | getBinString BOP_AND = "&&"
 | getBinString BOP_OR = "||"
 | getBinString BOP_COMMA = ","
;

fun 
   getUnString UOP_NOT = "!"
 | getUnString UOP_TYPEOF = "typeof"
 | getUnString UOP_MINUS = "-"
;

fun getBool bl = if bl then EXP_TRUE else EXP_FALSE;

fun 
   getBoolVal EXP_TRUE = true
 | getBoolVal EXP_FALSE = false
 | getBoolVal _  = error "trying to get boolval of non bool"
;

fun isBool bl = bl=EXP_TRUE orelse bl=EXP_FALSE;

fun typeError opr req fnd = 
   error ("operator '" ^ opr ^ "' requires " ^ req ^ ", found " ^ fnd)
;

fun binNumCheck a b = 
   case a of 
      EXP_NUM _ => 
         (case b of 
            EXP_NUM _ => true
          | _ => false)
    | _ => false     
;

fun binStringCheck a b =
   case a of
      EXP_STRING _ =>
         (case b of
            EXP_STRING _ => true
          | _ => false)
     | _ => false 

fun binBoolCheck a b = 
   case isBool a of
      true  =>
         (case isBool b of
            true => true
          | false => false)
     | false => false 
; 

fun binUndefinedCheck a b = 
   case a of
      EXP_UNDEFINED  =>
         (case b of
            EXP_UNDEFINED => true
          | _ => false)
     | _ => false 
; 

fun binSameCheck a b = 
   binBoolCheck a b orelse 
   binStringCheck a b orelse 
   binNumCheck a b orelse
   binUndefinedCheck a b
;

fun 
   unBoolCheck EXP_TRUE = true
 | unBoolCheck EXP_FALSE = true
 | unBoolCheck _ = false
;

fun
   unNumCheck (EXP_NUM _) = true
 | unNumCheck _ = false
;

fun doNot a = getBool ( not (getBoolVal a));

val numType = "number";
val boolType = "boolean";
val undefinedType = "undefined";
val stringType = "string";

fun 
   doTypeof (EXP_NUM _) = EXP_STRING numType
 | doTypeof EXP_TRUE = EXP_STRING boolType
 | doTypeof EXP_FALSE = EXP_STRING boolType
 | doTypeof EXP_UNDEFINED = EXP_STRING undefinedType
 | doTypeof (EXP_STRING _) = EXP_STRING stringType
 | doTypeof _ = error "unknown type!"
;

fun 
   getType (EXP_NUM _) = numType
 | getType EXP_TRUE = boolType
 | getType EXP_FALSE = boolType
 | getType EXP_UNDEFINED = undefinedType
 | getType (EXP_STRING _) = stringType
 | getType _ = error "unknown type!"
;

fun doMinus (EXP_NUM n) = EXP_NUM (~n)
  | doMinus _ = error "bad minus"
;

fun
   doNumBinary BOP_PLUS (EXP_NUM a) (EXP_NUM b) = EXP_NUM (a+b)
 | doNumBinary BOP_MINUS (EXP_NUM a) (EXP_NUM b) = EXP_NUM (a-b)
 | doNumBinary BOP_TIMES (EXP_NUM a) (EXP_NUM b) = EXP_NUM (a*b)
 | doNumBinary BOP_DIVIDE (EXP_NUM a) (EXP_NUM b) = EXP_NUM (Int.div(a,b))
 | doNumBinary BOP_MOD (EXP_NUM a) (EXP_NUM b) = EXP_NUM (Int.mod(a,b))
 | doNumBinary _ _ _ = error "not a num binary"
;

fun
   doRelBinary BOP_LT (EXP_NUM a) (EXP_NUM b) = getBool (a < b)
 | doRelBinary BOP_LE (EXP_NUM a) (EXP_NUM b) = getBool (a <= b)
 | doRelBinary BOP_GT (EXP_NUM a) (EXP_NUM b) = getBool (a > b) 
 | doRelBinary BOP_GE (EXP_NUM a) (EXP_NUM b) = getBool (a >= b) 
 | doRelBinary _ _ _ = error "not a eq binary"
;

fun 
   doEqBinary BOP_EQ a b = getBool (a =  b)
 | doEqBinary BOP_NE a b = getBool (a <> b)
 | doEqBinary _ _ _ = error "not a bool binary"
;

fun
   doStringBinary BOP_PLUS (EXP_STRING a) (EXP_STRING b) = EXP_STRING (a ^ b) 
 | doStringBinary _ _ _ = error "not a string binary"
;

fun printExpr exp = 
   let 
      val str = case exp of 
         EXP_NUM n => String.map (fn x => case x of #"~" => #"-" | n => n) (Int.toString n)
       | EXP_STRING n => "\"" ^ n ^ "\""
       | EXP_TRUE => "true"
       | EXP_FALSE => "false"
       | EXP_UNDEFINED => "undefined"
       | _ => "invalid final token"
   in
      str
   end
;
  
fun println str = 
   print (str ^ "\n") 
;

fun interpret fname =
   let
      val ast = parse fname
      val res = intProgram ast
   in
      (map println res; ())
   end

and intProgram (PROGRAM {elems=elems}) = 
   map intSourceElement elems

and intSourceElement (STMT {stmt=stmt}) = 
   intStatement stmt

and intStatement (ST_EXP {exp=exp}) = 
   (printExpression exp 0) ^ " ==> " ^ (printExpr (intExpression exp))

and 
   intExpression (EXP_BINARY n) = intBinary (EXP_BINARY n)
 | intExpression (EXP_UNARY n) = intUnary (EXP_UNARY n)
 | intExpression (EXP_COND n) = intCond (EXP_COND n)
 | intExpression n = n


and intBinary (EXP_BINARY {opr=opr, lft=lft, rht=rht}) = 
   let 
      val left = intExpression lft
      fun handlePlus () =
         let val right = intExpression rht in
            if binStringCheck left right
            then doStringBinary opr left right
            else if binNumCheck left right
            then doNumBinary opr left right
            else typeError "+" "number * number or string * string"
               ((getType left) ^ " * " ^ (getType right))

         end
      fun handleNum () = 
         let val right = intExpression rht in
            if binNumCheck left right
            then doNumBinary opr left right
            else typeError (getBinString opr) "number * number"
               ((getType left) ^ " * " ^ (getType right))
         end
      fun handleRel () =
         let val right = intExpression rht in
            if binNumCheck left right
            then doRelBinary opr left right
            else typeError (getBinString opr) "number * number"
               ((getType left) ^ " * " ^ (getType right))
         end
      fun handleEq () = 
         let val right = intExpression rht in 
            if binSameCheck left right
            then doEqBinary opr left right
            else case opr of
               BOP_EQ => EXP_FALSE
             | BOP_NE => EXP_TRUE
         end
      fun handleOr () = 
         if unBoolCheck left
         then case left of
            EXP_TRUE => EXP_TRUE
          | EXP_FALSE => 
               let val right = intExpression rht in
                  if unBoolCheck right
                  then right
                  else typeError "||" "boolean * boolean" 
                     ((getType left) ^ " * " ^ (getType right))
               end
            else typeError "||" "boolean" (getType left)
      fun handleAnd () = 
         if unBoolCheck left
         then case left of
            EXP_TRUE => 
               let val right = intExpression rht in 
                  if unBoolCheck right
                  then right
                  else typeError "&&" "boolean * boolean" 
                     ((getType left) ^ " * " ^ (getType right))
               end
          | EXP_FALSE => EXP_FALSE
          else typeError "&&" "boolean" (getType left)
      fun handleComma () = 
         intExpression rht
   in 
      case opr of
         BOP_PLUS => handlePlus ()
       | BOP_MINUS => handleNum ()
       | BOP_TIMES => handleNum ()
       | BOP_DIVIDE => handleNum ()
       | BOP_MOD => handleNum ()
       | BOP_EQ => handleEq ()
       | BOP_NE => handleEq ()
       | BOP_LT => handleRel ()
       | BOP_LE => handleRel ()
       | BOP_GT => handleRel ()
       | BOP_GE => handleRel ()
       | BOP_AND => handleAnd ()
       | BOP_OR => handleOr ()
       | BOP_COMMA => handleComma ()
   end

and intUnary (EXP_UNARY {opr=opr, opnd=opnd}) = 
   let
      val ret = intExpression opnd
      fun handleNot () =
         if unBoolCheck ret 
         then doNot ret
         else (print "unary "; typeError "!" "boolean" (getType opnd)) 
      fun handleTypeof () =
         doTypeof ret
      fun handleMinus () =
         if unNumCheck ret
         then doMinus ret
         else (print "unary "; typeError "-" "number" (getType opnd))
   in
      case opr of 
         UOP_NOT => handleNot ()
       | UOP_TYPEOF => handleTypeof ()
       | UOP_MINUS => handleMinus ()
   end

and intCond (EXP_COND {guard=guard, thenExp=thenExp, elseExp=elseExp}) =
   let val gd = intExpression guard
   in
      if unBoolCheck gd
      then if getBoolVal gd
         then
            intExpression thenExp
         else
            intExpression elseExp
      else error ("boolean guard required for 'cond' expression, found " ^ (getType gd))
   end 
;

