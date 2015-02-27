namespace IronJade
    type LexInnerTag=
        |BlockText
        |Inline of string
        |NestedInline of string
        |Normal
        |InnerLexTagError of string