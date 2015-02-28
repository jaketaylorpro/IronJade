namespace IronJade
    module LexLineBuilder=
        let buildLexLine (line:string) (inTextBlock:bool) :LexLine=
            if inTextBlock 
            then
                TextBlockLine(line) 
            else
                match line with
                | Util.ActivePattern.Prefix "doctype " rest -> DocType(rest)
                | Util.ActivePattern.Prefix "| " rest -> TextLine(rest)
                | _-> Tag(LexTagBuilder.buildLexTag line)