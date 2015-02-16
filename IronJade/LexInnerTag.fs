namespace IronJade
    module LexInnerTag=
        type LexInnerTag=
            |BlockText
            |Inline of string
            |Normal
            |InnerLexTagError of string
    
        let buildLexInnerTag (s:string) :LexInnerTag=
            match (s.TrimEnd()) with
            | "."-> BlockText
            | Util.Prefix " " rest -> Inline(rest)
            | "" -> Normal
            | _ -> InnerLexTagError(System.String.Format(Constants.Text.ERR_NO_MATCH_P1,s))