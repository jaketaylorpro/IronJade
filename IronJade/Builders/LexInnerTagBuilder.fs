namespace IronJade
    module LexInnerTagBuilder=
        let buildLexInnerTag (s:string) :LexInnerTag=
            match (s.TrimEnd()) with
            | "."-> BlockText
            | Util.Prefix " " rest -> Inline(rest)
            | Util.Prefix ": " rest -> NestedInline(rest)
            | "" -> Normal
            | _ -> InnerLexTagError(System.String.Format(Constants.Text.ERR_NO_MATCH_P1,s))