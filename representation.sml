(*PRINT INTERNAL REPRESENTATION IN PARSER*)
structure InternalRepresentation = struct


  datatype value = VInt of int
		 | VList of value list 

  and sentence = SEmpty
	       | SSequence of word * sentence
	       | SIf of sentence * sentence * sentence
	       | SWhile of sentence * sentence

  and word = WInt of int
           | WPrim of (value list -> value list)
           | WDefined of string

  fun stringOfValue (VInt i) = Int.toString i
    | stringOfValue (VList vs) = 
        "["^(String.concatWith "," (map stringOfValue vs))^"]"

  fun stringOfWord (WInt i) = Int.toString i
    | stringOfWord (WPrim _) = "<primitive>"
    | stringOfWord (WDefined w) = w

  fun stringOfSentence (SEmpty) = ""
    | stringOfSentence (SSequence (w,s)) = String.concat [stringOfWord w,
							  " ",
							  stringOfSentence s]
    | stringOfSentence (SIf (s1,s2,s3)) = String.concat ["IF ",
							 stringOfSentence s1,
							 "ELSE ",
							 stringOfSentence s2,
							 "THEN ",
							 stringOfSentence s3]
    | stringOfSentence (SWhile (s1,s2)) = String.concat ["WHILE ",
							 stringOfSentence s1,
							 "REPEAT ",
							 stringOfSentence s2]


  fun stringOfStack [] _ = ""
    | stringOfStack _ 0 = "..."
    | stringOfStack (v::vs) d = String.concat [stringOfValue v, " ", 
					       stringOfStack vs (d-1)]
		       
end
