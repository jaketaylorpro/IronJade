﻿namespace IronJade
    module LexLineBuilder=
        let buildLexLine (line:string) (inTextBlock:bool) :LexLine=
            if inTextBlock 
            then
                TextBlockLine(line) 
            else
                match line with
                | Util.Prefix "doctype " rest -> DocType(rest)
                | Util.Prefix "| " rest -> TextLine(rest)
                | "//" -> Tag(LexTagBuilder.buildBlockCommentLexTag)
                | "//-" -> Tag(LexTagBuilder.buildHiddenBlockCommentLexTag)
                | Util.Prefix "// " rest -> Tag(LexTagBuilder.buildCommentLexTag rest)
                | Util.Prefix "//- " rest -> Tag(LexTagBuilder.buildHiddenCommentLexTag rest)
                | _-> Tag(LexTagBuilder.buildLexTag line)