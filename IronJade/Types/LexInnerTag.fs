namespace IronJade
    type LexInnerTag=
        |BlockText
        |Inline of string
        |Normal
        |InnerLexTagError of string