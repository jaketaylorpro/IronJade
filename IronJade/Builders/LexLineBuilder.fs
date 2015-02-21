namespace IronJade
    module LexLineBuilder=
        let buildLexLine (line:string) (inTextBlock:bool) :LexLine=
            if inTextBlock 
            then
                TextBlockLine(line) 
            else
                match line with
                | Util.Prefix "doctype " rest -> DocType(rest)
                | Util.Prefix "| " rest -> TextLine(rest)
                | Util.Prefix "//" rest -> Tag(LexTagBuilder.buildCommentLexTag rest)
                | _-> Tag(LexTagBuilder.buildLexTag line)