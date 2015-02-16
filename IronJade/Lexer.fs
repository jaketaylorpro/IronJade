namespace IronJade
    module Lexer=
        let lexLines (lines:seq<string>) :LexNode.LexNode=
            let readLines (lines:seq<string>) :List<int*int*string>=
                lines //read each line, assign it a number, and an indentation
                |>Seq.mapi (fun lineNumber l->let tabCount=l.ToCharArray()|>Seq.takeWhile (fun c->c='\t')|>Seq.length
                                              let line= l.TrimStart()
                                              lineNumber,tabCount,line)
                |>Seq.toList
            let rec groupLines (lines:List<int*int*string>) (nodes:List<LexNode.LexNode>) (inTextBlock:bool) :List<LexNode.LexNode>=
                match lines with
                | [] -> //done, no lines left to process
                    nodes
                | (ln,ind,line)::_ ->
                    let lexLine=LexLine.buildLexLine line inTextBlock
                    let indentedLines,restLines=
                        lines.Tail
                        |>Util.takeWhileAndRest (fun (_,i,_) -> i>ind)
                    match lexLine with
                    //first handle non nestible types
                    | LexLine.Root //root should not appear here so we'll treat it as not nestble
                    | LexLine.DocType(_) //doctype shouldn't be nested
                    | LexLine.TextLine(_) //a pipe-prefixed line of text shouldn't be nested
                    | LexLine.Tag(LexTag.LexTagProper({Name=_;Attributes=_;LexInnerTag=LexInnerTag.Inline(_)})) //a tag with inline text can't be nested
                    | LexLine.Tag(LexTag.LexTagError(_)) //a tag with an error will have it's indented children ignored
                    | LexLine.Tag(LexTag.LexTagInnerError(_)) //a tag with an error will have it's indented children ignored
                    | LexLine.Tag(LexTag.LexTagProper({Name=_;Attributes=_;LexInnerTag=LexInnerTag.InnerLexTagError(_)})) //a tag with an inner error will have it's indented children ignored
                        -> groupLines restLines ({LexLine=lexLine;ChildNodes=[];LineNumber=ln;Indentation=ind}::nodes) false
                    //now handle nestabile types
                    | LexLine.Tag(LexTag.LexTagProper({Name=_;Attributes=_;LexInnerTag=normalOrBlock}))
                        -> let childNodes=groupLines indentedLines [] (normalOrBlock = LexInnerTag.BlockText)
                           groupLines restLines ({LexLine=lexLine;ChildNodes=childNodes;LineNumber=ln;Indentation=ind}::nodes) false
                    //now handle text block lines
                    | LexLine.TextBlockLine(s)
                        -> let childNodes=groupLines indentedLines [] true
                           groupLines restLines ({LexLine=lexLine;ChildNodes=childNodes;LineNumber=ln;Indentation=ind}::nodes) true
            {LexLine=LexLine.Root;ChildNodes=(groupLines (readLines lines) [] false);LineNumber=(-1);Indentation=(-1)}

        