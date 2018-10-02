
structure Parser =  struct

  (*
   *  Wrapper around the regexp library
   *)      

  structure R = RegExpFn (structure P = AwkSyntax structure E = DfaEngine)

                
  (* match a compiled regular expression against a list of characters *)
                
  fun matchRE' re cs = let
    val prefix = R.prefix re List.getItem
    fun getMatch NONE = NONE
      | getMatch (SOME (mt, cs')) = let
          val {pos,len} = MatchTree.root mt
        in
          SOME (implode (List.take (pos,len)), cs')
        end
  in
    getMatch (prefix cs)
  end
                      
  (* match a string regular expression against a list of characters *)
                       
  fun matchRE re cs = matchRE' (R.compileString re) cs

  exception Parsing of string
  exception Unknown_Primitive 
  fun unknownPrim prim = let val _ = TextIO.print ("unrecognized primitive "^prim) in raise Unknown_Primitive end
  fun parseError msg = raise Parsing msg
                         
  fun upper w = implode (map Char.toUpper (explode w))                       
  fun countOps ops = 4*(List.length (foldr (op @) [] ops)) 


  (* 
   *   A simple lexer
   *)

  datatype token = T_INT of int
                 | T_WORD of string
		 | T_COLON
		 | T_SEMI
		 | T_IF
		 | T_THEN
		 | T_ELSE
		 | T_WHILE
		 | T_REPEAT
         | T_LBRACK
         | T_RBRACK
         | T_LBRACE
         | T_RBRACE


  fun stringOfToken (T_WORD s) = "T_WORD["^s^"]"
    | stringOfToken (T_INT i) = "T_INT["^(Int.toString i)^"]"
    | stringOfToken (T_COLON) = "T_COLON"
    | stringOfToken (T_SEMI) = "T_SEMI"
    | stringOfToken (T_IF) = "T_IF"
    | stringOfToken (T_THEN) = "T_THEN"
    | stringOfToken (T_ELSE) = "T_ELSE"
    | stringOfToken (T_WHILE) = "T_WHILE"
    | stringOfToken (T_REPEAT) = "T_REPEAT"
    | stringOfToken (T_LBRACK) = "T_LBRACK"
    | stringOfToken (T_RBRACK) = "T_RBRACK"
    | stringOfToken (T_RBRACE) = "T_RBRACE"
    | stringOfToken (T_LBRACE) = "T_LBRACE"
                   
  fun whitespace _ = NONE
                     

  fun produceInt text = (case Int.fromString text
                          of NONE => parseError "integer literal out of bounds"
                           | SOME i => SOME (T_INT i))

  fun produceColon _ = SOME (T_COLON)
  fun produceSemi _ = SOME (T_SEMI)
  fun produceLbrack _ = SOME (T_LBRACK)
  fun produceRbrack _ = SOME (T_RBRACK)
  fun produceLbrace _ = SOME (T_LBRACE)
  fun produceRbrace _ = SOME (T_RBRACE)

  fun produceWord "if" = SOME (T_IF)
    | produceWord "then" = SOME (T_THEN)
    | produceWord "else" = SOME (T_ELSE)
    | produceWord "while" = SOME (T_WHILE)
    | produceWord "repeat" = SOME (T_REPEAT)
    | produceWord text = SOME (T_WORD text)
                        
  val tokens = let 
    fun convert (re,f) = (R.compileString re, f)
  in
    map convert [("( |\\n|\\t)+",         whitespace),
		         (":",                    produceColon),
		         (";",                    produceSemi),
                 (">",                    produceRbrack),
                 ("<",                    produceLbrack),
                 ("{",                    produceLbrace),
                 ("}",                    produceRbrace),
                 ("~?[0-9]+[^ \\n\\t0-9]+",  produceWord),
                 ("~?[0-9]+",             produceInt),
                 ("[^ \\n\\t]+",          produceWord)]
  end

  fun convert sym =
      (case sym 
         of "0=" => "ZEROEQ"
          | "0>" => "GREATER0"
          | "+" => "ADD"
          | "-" => "SUBTRACT"
          | "*" => "MUL"
          | _ => sym)
              (*write pre lexer which explodes it and if shit matches a special
              * character. If it does implde previous word and append it alone
              * to list*) 
               
  fun getToken cs = let
    fun loop [] = parseError ("cannot tokenize "^(implode cs))
      | loop ((re,f)::xs) = (case matchRE' re cs
                              of NONE => loop xs
                               | SOME (m,cs') => (f m,cs'))
  in
    loop tokens
  end
                    
                    
  fun lex []  = []
    | lex cs = let
        val (token,cs') = getToken cs
      in
        case token 
         of NONE => lex cs'
          | SOME t => t::(lex cs')
      end
               
               
  fun lexString str = lex (explode str)

  (* 
   *   A SIMPLE PARSER FOR A FORTH-LIKE LANGUAGE
   *
   *   Grammar:
   *
   *   decl ::= T_COLON T_WORD sentence
   *            sentence
   *
   *   sentence ::= T_IF sentence T_THEN sentence
   *                T_IF sentence T_ELSE sentence T_THEN sentence
   *                T_WHILE sentence T_REPEAT sentence (not implemented)
   *                word sentence
   *                word
   *                <empty>
   *
   *   word ::= T_INT
   *            T_WORD
   *)

  fun expect_INT ((T_INT i)::ts) = SOME (i,ts)
    | expect_INT _ = NONE

  fun expect_WORD ((T_WORD s)::ts) = SOME (s,ts)
    | expect_WORD _ = NONE

  fun expect_COLON (T_COLON::ts) = SOME ts
    | expect_COLON _ = NONE

  fun expect_SEMI (T_SEMI::ts) = SOME ts
    | expect_SEMI _ = NONE

  fun expect_IF (T_IF::ts) = SOME ts
    | expect_IF _ = NONE

  fun expect_THEN (T_THEN::ts) = SOME ts
    | expect_THEN _ = NONE

  fun expect_ELSE (T_ELSE::ts) = SOME ts
    | expect_ELSE _ = NONE

  fun expect_WHILE (T_WHILE::ts) = SOME ts
    | expect_WHILE _ = NONE

  fun expect_REPEAT (T_REPEAT::ts) = SOME ts
    | expect_REPEAT _ = NONE

  fun expect_LBRACK (T_LBRACK::ts) = SOME ts
    | expect_LBRACK _ = NONE

  fun expect_RBRACK (T_RBRACK::ts) = SOME ts
    | expect_RBRACK _ = NONE

  fun expect_LBRACE (T_LBRACE::ts) = SOME ts
    | expect_LBRACE _ = NONE

  fun expect_RBRACE (T_RBRACE::ts) = SOME ts
    | expect_RBRACE _ = NONE
    
    (*
    * The parser propper
    * different expressions are matched against token combinations
    * includes parsing of assembly library 
    *)
   
  (* parse arbitrary assembly into single type *)
  and parseASM ts =
      (case expect_WORD ts
      of NONE => 
         (case expect_INT ts 
         of NONE => NONE
          | SOME (i,ts) => 
              (case parseASM ts
              of NONE => SOME ([Int.toString i],ts)
               | SOME (words,tss) => SOME ((Int.toString i)::words,tss))) 
       | SOME (word,ts') => 
           (case parseASM ts'
           of NONE => SOME ([word],ts')
            | SOME (words,ts'') => SOME(word::words,ts'')))
(* parse name of assembly primative from library *)
  and parseFunDec ts = 
      (case expect_COLON ts
      of NONE => NONE
       | SOME ts =>
           (case expect_WORD ts
           of NONE => NONE
            | SOME (name, ts) => SOME ([name],ts)))

  (* parse assembly implementation of language primitive from library *)
  and parseFunDef ts = 
      (case expect_LBRACK ts
      of NONE => NONE
       | SOME ts =>
           (case parseASM ts 
           of NONE => NONE
            | SOME (asm,ts) => 
                (case expect_RBRACK ts
                of NONE => NONE
                 | SOME ts =>
                     (case parseFunDef ts
                     of NONE => SOME ([asm], ts)
                      | SOME (ops, tss) => SOME (asm::ops,tss)))))
		
  (* parse assembly library as whole *)
  and parseLib ts =
      (case parseFunDec ts
      of NONE => NONE
       | SOME (name,ts) =>
           (case parseFunDef ts
           of NONE => NONE
            | SOME (funk,ts) => 
                (case parseLib ts
                of NONE => SOME([name::funk],ts)
                 | SOME (primitive,ts) => SOME((name::funk)::primitive,ts))))
  

  (* match internal representation to associated library implementation *)
  and lookup primitive lib =
  let
    fun matchPrim prim [] = unknownPrim prim 
      | matchPrim prim (asm::env) = 
      let
          fun modPush (name::arg::call) s = (name::(arg@[","^String.extract (s,4,NONE)])::call)
          fun matchName comp (name::impl) =
            if name = ["PUSH"]
                then 
                  if (String.isSubstring "PUSH" prim) then true else false
                else
                  if [comp] = name then true else false
        in
          if (matchName prim asm) 
          then 
            if (String.isSubstring "PUSH" prim) then (List.tl (modPush asm prim)) else (List.tl asm)
          else matchPrim prim env
        end
    in 
      matchPrim primitive lib 
  end

  (* parse if then else statement into IR *)
  and parseIf ts lib=
  (case expect_IF ts
  of NONE => NONE
   | SOME ts => 
       (case parse2Sym ts
       of NONE => NONE
        | SOME (ifOps,ts) => 
            (case expect_ELSE ts
            of NONE=> 
                (case expect_THEN ts
                of NONE => NONE
                 | SOME ts =>
                     (case parse2Sym ts
                     of SOME (thenOps,ts) => 
                     let 
                       val builtIf = map (fn x => lookup x lib) (map convert ifOps) 
                       val ifCount = countOps builtIf
                     in
                       SOME((["IF"]@ifOps@["ELSE"]@thenOps),ts,[ifCount,ifCount])
                     end
                      | NONE => 
                     let 
                       val builtIf = map (fn x => lookup x lib) (map convert ifOps) 
                       val ifCount = countOps builtIf
                     in
                       SOME ((["IF"]@ifOps@["ELSE"]),ts,[ifCount,ifCount])
                     end
                     ))
             | SOME ts =>
                 (case parse2Sym ts
                 of NONE =>
                      (case expect_THEN ts
                      of NONE => NONE
                       | SOME ts =>
                           (case parse2Sym ts
                           of SOME (thenOps,ts) => 
                           let 
                             val builtIf = map (fn x => lookup x lib) (map convert ifOps) 
                             val ifCount = (countOps builtIf) +4
                           in
                           SOME((["IF"]@ifOps@["ELSE"]@thenOps),ts,[ifCount,ifCount])
                           end
                            | NONE => 
                             let 
                               val builtIf = map (fn x => lookup x lib) (map convert ifOps) 
                               val ifCount = (countOps builtIf) +4
                             in
                                SOME ((["IF"]@ifOps@["ELSE"]),ts,[ifCount,ifCount])
                             end
                            ))
                  | SOME (elseOps,ts)=>
                     (case expect_THEN ts
                      of NONE => NONE
                       | SOME ts =>
                           (case parse2Sym ts
                           of SOME (thenOps,ts) => 
                           let 
                             val builtIf = map (fn x => lookup x lib) (map convert ifOps) 
                             val builtElse = map (fn x => lookup x lib) (map convert elseOps) 
                             val ifCount = (countOps builtIf)+4
                             val elseCount = countOps builtElse
                           in
                           SOME((["IF"]@ifOps@["ELSE"]@elseOps@thenOps),ts,[ifCount,elseCount])
                           end
                            | NONE => 
                           let 
                             val builtIf = map (fn x => lookup x lib) (map convert ifOps) 
                             val builtElse = map (fn x => lookup x lib) (map convert elseOps) 
                             val ifCount = (countOps builtIf)+4
                             val elseCount = countOps builtElse
                           in
                                SOME ((["IF"]@ifOps@["ELSE"]@elseOps),ts,[ifCount,elseCount])
                           end
                            ))))))

  (* assemble processed library into format of internal environment *)
  and buildDecl funkRaw lib=
    let
        val functionsPrim = map (fn x => List.nth (x,1)) funkRaw 
        val functionsNames = map (fn x => List.nth (x,0)) funkRaw
        val functionsConv = map (fn x => map convert x) functionsPrim
        val functionsBuilt = map (fn x => foldr (op @) [] x)(map (fn x => build x lib) functionsConv)
        val functionsLib = combine functionsNames functionsBuilt (List.length
         functionsNames)
    in
      functionsLib
    end
  (* wraps lookup *)
  and build (prim::prims) lib =
    (lookup prim lib)::(build prims lib)
    | build [] lib = []
    
  (* list manip helper *)
  and combine x y 0 = []
    | combine x y n = [(List.nth (x,(n-1)))::List.nth (y,(n-1))]@(combine x y (n-1))

  (* parse function definition in forth *)
  and parseDecl ts =
  (case expect_COLON ts 
  of NONE => NONE
   | SOME ts => 
       (case expect_WORD ts
       of NONE => NONE
        | SOME (w,ts) =>
            (case expect_LBRACE ts
            of NONE => NONE
             | SOME ts =>
                (case parse2Sym ts 
                of NONE => NONE
                | SOME (funk,ts) =>
                   (case expect_RBRACE ts
                   of NONE => NONE
                    | SOME ts => SOME ([[upper w],funk],ts))))))
  (* parse out numbers and words into IR *)
  and parse2Sym ts =
  (case expect_INT ts
  of SOME (i,ts) =>
        (case parse2Sym ts
        of NONE => SOME (["PUSH "^(Int.toString i)],ts)
         | SOME (syms,ts) => SOME(("PUSH "^(Int.toString i))::syms,ts))
   | NONE => 
       (case expect_WORD ts
       of SOME (w,ts) =>
         (case parse2Sym ts
         of NONE => SOME ([upper w],ts)
          | SOME (syms, ts) => SOME ((upper w)::syms,ts))
        | NONE => NONE))
  (* parse a program *)
  and parse [] program functions jmpArgs lib= (program, functions, jmpArgs)
    | parse ts program functions jmpArgs lib=
    (case parse2Sym ts 
    of SOME (text,ts) => parse ts (program@text) functions jmpArgs lib
     | NONE =>
         (case parseDecl ts
         of SOME (decl,ts) => parse ts program (decl::functions) jmpArgs (lib@(buildDecl [decl] lib))
          | NONE =>
              (case parseIf ts lib
              of NONE => unknownPrim (stringOfToken (hd ts))
               | SOME (ifOps, ts, count) => parse ts (program @ ifOps) functions (jmpArgs@count) lib))) 
  
end
