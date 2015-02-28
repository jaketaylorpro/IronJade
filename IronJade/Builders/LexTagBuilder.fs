namespace IronJade
    module LexTagBuilder=
        let buildLexTag (line:string) :LexTag=
            let buildCommentLexTag (s:string)=
                LexTagProper({Name=("//");Attributes=[];LexInnerTag=LexInnerTag.Inline(s.Trim())})
            let buildHiddenCommentLexTag (s:string)=
                LexTagProper({Name=("//-");Attributes=[];LexInnerTag=LexInnerTag.Inline(s.Trim())})
            let buildBlockCommentLexTag=            
                LexTagProper({Name=("//");Attributes=[];LexInnerTag=LexInnerTag.BlockText})
            let buildHiddenBlockCommentLexTag=
                LexTagProper({Name=("//-");Attributes=[];LexInnerTag=LexInnerTag.BlockText})
            match line with
            | "//" -> buildBlockCommentLexTag
            | "//-" -> buildHiddenBlockCommentLexTag
            | Util.ActivePattern.Prefix "// " rest -> buildCommentLexTag rest
            | Util.ActivePattern.Prefix "//- " rest -> buildHiddenCommentLexTag rest
            | _ ->
                  let compileAttributes (id:Option<List<string>>) (classes:Option<List<string>>) (attrN:Option<List<string>>) (attrV:Option<List<string>>) :List<string*Option<string>>=
                      let idPair=match id with //TODO handle additional cases with error types
                                 |None | Some([]) -> []
                                 |Some([idv]) ->[("id",Some(idv))]
                      let classPair=match classes with
                                    |None | Some([]) -> []
                                    |Some(classesv) ->[("class",Some(List.fold (fun acc c->if acc="" then c else acc+" "+c) "" classesv))]
                      let attrPairs=match attrN with
                                    |None |Some([]) -> []
                                    |Some(_) ->List.zip attrN.Value (attrV.Value|>List.map Util.String.ifSomeTrimmed)
                      List.concat [idPair;classPair;attrPairs]

                  let matchMap=Util.String.getRegexGroupsByName Constants.Regex.FullTagPattern line
                  match matchMap with
                  | None -> LexTagError(System.String.Format(Constants.Text.ERR_NO_MATCH_P1,line))
                  | Some(map) -> let nameMatches=Map.tryFind Constants.Regex.GroupName map
                                 let name= match nameMatches with //TODO handle additional cases with error types
                                           | None|Some([]) -> "div"
                                           | Some([s]) -> s //no need to support other matches on name, regex can only get one capture for the name pattern
                                 let attributes=(compileAttributes 
                                                  (Map.tryFind Constants.Regex.GroupId map) 
                                                  (Map.tryFind Constants.Regex.GroupClass map) 
                                                  (Map.tryFind Constants.Regex.GroupAttrn map) 
                                                  (Map.tryFind Constants.Regex.GroupAttrv map))
                                 let textMatches=(Map.tryFind Constants.Regex.GroupText map)
                                 let hasNestedTag= match (Map.tryFind Constants.Regex.GroupColon map) with //TODO handle error cases
                                                   | None | Some([]) -> false
                                                   | Some([":"]) -> true
                                 let hasBlockText= match (Map.tryFind Constants.Regex.GroupDot map) with //TODO handle error cases
                                                   | None | Some([]) -> false
                                                   | Some(["."]) -> true
                                 let text=match textMatches with //TODO handle additional cases with error types
                                          | None | Some([]) -> ""
                                          | Some([s]) ->s
                                 let innerLexTag= LexInnerTagBuilder.buildLexInnerTag text hasNestedTag hasBlockText
                                 match innerLexTag with
                                 |LexInnerTag.InnerLexTagError(s) -> LexTagInnerError(s)
                                 | _ -> LexTagProper({Name=name;Attributes=attributes;LexInnerTag=innerLexTag})
        