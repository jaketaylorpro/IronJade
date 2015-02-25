namespace IronJade
    module LexTagBuilder=
        let buildLexTag (line:string)=
            let compileAttributes (id:Option<List<string>>) (classes:Option<List<string>>) (attrN:Option<List<string>>) (attrV:Option<List<string>>) :List<string*Option<string>>=
                let idPair=match id with //TODO handle additional cases with error types
                           |None | Some([]) -> []
                           |Some([idv]) ->[("id",Some(idv))]
                let classPair=match classes with
                              |None | Some([]) -> []
                              |Some(classesv) ->[("class",Some(List.fold (fun acc c->if acc="" then c else acc+" "+c) "" classesv))]
                let attrPairs=match attrN with
                              |None |Some([]) -> []
                              |Some(_) ->List.zip attrN.Value (attrV.Value|>List.map Util.ifSomeTrimmed)
                List.concat [idPair;classPair;attrPairs]
            let matchMap=[Constants.Regex.FULL_TAG_PATTERN;Constants.Regex.FULL_DIV_ID_PATTERN;Constants.Regex.FULL_DIV_CLASS_PATTERN]
                         |>List.map (fun p-> Util.getRegexGroupsByName p line)
                         |>List.tryPick (fun m->m)
            match matchMap with
            | None -> LexTagError(System.String.Format(Constants.Text.ERR_NO_MATCH_P1,line))
            | Some(map) -> let nameMatches=Map.tryFind Constants.Regex.GROUP_NAME map
                           let name= match nameMatches with //TODO handle additional cases with error types
                                     | None|Some([]) -> "div"
                                     | Some([s]) -> s //no need to support other matches on name, regex can only get one capture for the name pattern
                           let attributes=(compileAttributes 
                                            (Map.tryFind Constants.Regex.GROUP_ID map) 
                                            (Map.tryFind Constants.Regex.GROUP_CLASS map) 
                                            (Map.tryFind Constants.Regex.GROUP_ATTRN map) 
                                            (Map.tryFind Constants.Regex.GROUP_ATTRV map))
                           let textMatches=(Map.tryFind Constants.Regex.GROUP_TEXT map)
                           let innerLexTag= LexInnerTagBuilder.buildLexInnerTag (match textMatches with //TODO handle additional cases with error types
                                                                                 | None|Some([]) -> ""
                                                                                 | Some([s]) ->s)
                           match innerLexTag with
                           |LexInnerTag.InnerLexTagError(s) -> LexTagInnerError(s)
                           | _ -> LexTagProper({Name=name;Attributes=attributes;LexInnerTag=innerLexTag})
        let buildCommentLexTag (s:string)=
            LexTagProper({Name=("//");Attributes=[];LexInnerTag=LexInnerTag.Inline(s.Trim())})
        let buildHiddenCommentLexTag (s:string)=
            LexTagProper({Name=("//-");Attributes=[];LexInnerTag=LexInnerTag.Inline(s.Trim())})
        let buildBlockCommentLexTag=            
            LexTagProper({Name=("//");Attributes=[];LexInnerTag=LexInnerTag.BlockText})
        let buildHiddenBlockCommentLexTag=
            LexTagProper({Name=("//-");Attributes=[];LexInnerTag=LexInnerTag.BlockText})
        