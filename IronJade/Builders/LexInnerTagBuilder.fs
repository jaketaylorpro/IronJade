namespace IronJade
    module LexInnerTagBuilder=
        let buildLexInnerTag (s:string) hasNestedTag hasBlockText :LexInnerTag=
            match hasNestedTag,hasBlockText,(s.TrimEnd()) with
            | true,_,rest-> NestedInline(rest)
            | _,true,_ -> BlockText
            | _,_,"" -> Normal
            | _,_,rest -> Inline(rest)