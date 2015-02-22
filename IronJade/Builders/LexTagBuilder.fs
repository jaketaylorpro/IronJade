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
            let matchMap=Util.getRegexGroupsByName Constants.Regex.FULL_TAG_PATTERN line
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
        let buildCommentLexTag s=
            match s with
            | ""|"-" -> 
                LexTagProper({Name=("//"+s);Attributes=[];LexInnerTag=LexInnerTag.BlockText})
            | Util.Regex "(?:(- )|( ))(.*)" [prefix;suffix] -> 
                let tag=if suffix = "" then LexInnerTag.BlockText else LexInnerTag.Inline(suffix)
                LexTagProper({Name=("//"+prefix);Attributes=[];LexInnerTag=tag})
            | _ ->
                LexTagError(System.String.Format(Constants.Text.ERR_NO_MATCH_P1,"//s"))