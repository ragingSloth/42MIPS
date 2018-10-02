structure Compiler = struct
  structure P = Parser

  (* string manip, replaces placeholders *)
  fun replaceArgs []  _ result = List.rev result
    | replaceArgs (opc::asm) (arg::args) result = 
    (case opc
    of "@" => replaceArgs asm args ((Int.toString arg)::result)
     | _ => replaceArgs asm (arg::args) (opc::result))
    | replaceArgs (opc::asm) [] results = replaceArgs asm [] (opc::results)

  (* file IO *)
  fun readf fName = 
      let 
        val f = TextIO.openIn(fName)
        val content = TextIO.inputAll(f)
      in
        TextIO.closeIn(f);
        content
      end
  (* lex plaintext for use in parser *)
  fun lexLib fName =
        let 
          val f = readf fName
        in
          P.lexString f
        end
  (* build IR using parser *)
  fun file2Sym fName lib =
        let 
          val f = readf fName
          val ts = P.lexString f
          val internalSyntax = P.parse ts [] [] [] lib
        in
          internalSyntax
        end
  (* convert IR to assembly *)      
  fun build (prim::prims) lib =
    (P.lookup prim lib)::(build prims lib)
    | build [] lib = []

  (* compile the program as a whole *)
  fun write target fIn libASM =
    let 
        fun concatOp opc= (foldr (fn (x, y) => x^" "^y) "" opc);

        val outFile = target^".asm"
        val f = TextIO.openOut(outFile)
        val lib = 
        (case P.parseLib(lexLib libASM) 
        of NONE => []
         | SOME (library,ts) => library)
        val fileRaw = 
          (case file2Sym fIn lib
           of (program, functions, ifArgs) => {prog=program, funk=functions,
           ifArgs=ifArgs})
        val file = map Parser.convert (#prog fileRaw)
        (* this is the meat of it, intermediate forms are succesively built into valid assembly *)
        val functionsLib = Parser.buildDecl (#funk fileRaw) lib
        val assemblyBlocks = foldr (op @) [] (build file (functionsLib@lib))
        val assemblyNoArgs = foldr (op @) [] (map (fn x => x@["\n"]) assemblyBlocks)
        val assembly = concatOp (replaceArgs assemblyNoArgs (#ifArgs fileRaw) [])
        fun writeOut args =
        let
          fun writeFile strs =
            let val _ = TextIO.output(f,(".text\nla $t1, stack1\nadd $t1,$t1,396\n"^strs^"\n"))
            in 
            TextIO.output(f,(readf "MIPSbackend.asm"))
            end
        in
          writeFile args;
          TextIO.closeOut f
        end
    in
      writeOut assembly
    end
end
