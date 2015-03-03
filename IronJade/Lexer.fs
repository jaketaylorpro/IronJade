namespace IronJade
open System.Text.RegularExpressions
    module Lexer=
        let reindentNestedTextBlockLines (lines:List<PreLexLine>) (indType:Indentation)=
            let replacementIndentation=match indType with
                                       |Indentation.Space(n) -> new System.String(' ',n)
                                       |Indentation.Tab -> "\t"
            lines|>List.map (fun {RawText=rawText;LineNumber=ln;Indentation=ind;TrimmedText=text}->{RawText=rawText;LineNumber=ln;Indentation=(ind-1);TrimmedText=(replacementIndentation+text)})
        let detectIndentationType (lines:seq<string>)= //TODO improve this
            let firstIndentedLine= lines|>Seq.tryFind (fun line ->
                                                                    let fc=line.Chars 0 //f# is lazy so this should only be evaled if line<>""
                                                                    line<>"" && (fc='\t'||fc=' '))
            match firstIndentedLine with
                |None -> Tab
                |Some(line) when (line.Chars 0) = '\t' -> Tab
                |Some(line) -> let numSpaces=line.ToCharArray()|>Seq.takeWhile (fun c->c=' ')|>Seq.length
                               Space(numSpaces)
        (*let handleIndentationErrors (lines:seq<string>) (ind:Indentation)=
            match ind with
            |Indentation.Space(_) //assert that no tabs are used for indentation in the document
                -> lines|>Seq.forall(fun l-> Regex.Match(l,s)*)
        let readLines (lines:seq<string>) (indType:Indentation) :List<PreLexLine>=
                lines //read each line, assign it a number, and an indentation
                |>Seq.mapi (fun lineNumber l->let tabCount,line= match indType with
                                                                 | Indentation.Tab ->
                                                                     let totalTabs=l.ToCharArray()|>Seq.takeWhile (fun c->c='\t')|>Seq.length
                                                                     totalTabs,l.Substring(totalTabs)
                                                                 | Indentation.Space(n) ->
                                                                     let totalSpaces=l.ToCharArray()|>Seq.takeWhile (fun c->c=' ')|>Seq.length
                                                                     let totalTabs=totalSpaces/n
                                                                     totalTabs,l.Substring(totalTabs*n)
                                              {RawText=l;LineNumber=lineNumber;Indentation=tabCount;TrimmedText=line})
                |>Seq.toList
        let rec groupLines (lines:List<PreLexLine>) (nodes:List<LexNode>) (inTextBlock:bool) (indType:Indentation) :List<LexNode>=
                match lines with
                | [] -> //done, no lines left to process
                    nodes|>List.rev
                | {RawText=_;LineNumber=ln;Indentation=ind;TrimmedText=line}::_ ->
                    let lexLine=LexLineBuilder.buildLexLine line inTextBlock
                    let indentedLines,restLines=
                        lines.Tail
                        |>Util.List.takeWhileAndRest (fun {RawText=_;LineNumber=_;Indentation=i;TrimmedText=_} -> i>ind)
                    match lexLine with
                    //first handle non nestible types
                    | LexLine.Root(_) //root should not appear here so we'll treat it as not nestble
                    | LexLine.DocType(_) //doctype shouldn't be nested
                    | LexLine.TextLine(_) //a pipe-prefixed line of text shouldn't be nested
                    | LexLine.Tag(LexTag.LexTagProper({Name=_;Attributes=_;LexInnerTag=LexInnerTag.Inline(_)})) //a tag with inline text can't be nested
                    | LexLine.Tag(LexTag.LexTagError(_)) //a tag with an error will have it's indented children ignored
                    | LexLine.Tag(LexTag.LexTagInnerError(_)) //a tag with an error will have it's indented children ignored
                    | LexLine.Tag(LexTag.LexTagProper({Name=_;Attributes=_;LexInnerTag=LexInnerTag.InnerLexTagError(_)})) //a tag with an inner error will have it's indented children ignored
                        -> groupLines restLines ({LexLine=lexLine;ChildNodes=[];LineNumber=ln;Indentation=ind}::nodes) false indType
                    //handle the inline nested case
                    | LexLine.Tag(LexTag.LexTagProper({Name=_;Attributes=_;LexInnerTag=LexInnerTag.NestedInline(s)}))
                        -> let childChildNodes=groupLines indentedLines [] false indType
                           //TODO now to recursivly add until we find a non nested tag
                           //recursivly create new lexlines with the remaining text of the line, until we don't detect a nested tag. cons all the lexlines onto a list
                           let childNode=Util.List.foldWhile (fun st ns -> 
                                                                          let lexLine=LexLineBuilder.buildLexLine st false
                                                                          match lexLine with
                                                                          | LexLine.Tag(LexTag.LexTagProper({Name=_;Attributes=_;LexInnerTag=LexInnerTag.NestedInline(s2)}))
                                                                            -> Some(s2),lexLine::ns
                                                                          | _ -> None,lexLine::ns) s []
                                         //next nest each tag
                                         |>List.fold (fun runningChildNodes l -> [{LexLine=l;ChildNodes=runningChildNodes;LineNumber=ln;Indentation=ind}]) childChildNodes
                                         |>List.head
                           //let childNode={LexLine=LexLine.Tag(LexTagBuilder.buildLexTag s);ChildNodes=childChildNodes;LineNumber=ln;Indentation=ind+1}
                           groupLines restLines ({LexLine=lexLine;ChildNodes=[childNode];LineNumber=ln;Indentation=ind}::nodes) false indType
                    //now handle nestabile types
                    | LexLine.Tag(LexTag.LexTagProper({Name=_;Attributes=_;LexInnerTag=normalOrBlock}))
                        -> let childNodes=groupLines indentedLines [] (normalOrBlock = LexInnerTag.BlockText) indType
                           groupLines restLines ({LexLine=lexLine;ChildNodes=childNodes;LineNumber=ln;Indentation=ind}::nodes) false indType
                    //now handle text block lines
                    | LexLine.TextBlockLine(s)
                        -> groupLines (List.append (reindentNestedTextBlockLines indentedLines indType) restLines) ({LexLine=lexLine;ChildNodes=[];LineNumber=ln;Indentation=ind}::nodes) true indType
        let lexLines (lines:seq<string>) :LexNode=
            let indType=detectIndentationType lines
            let linesR=readLines lines indType
            {
                LexLine=LexLine.Root(indType,NotCompiled);
                ChildNodes=(groupLines linesR [] false indType);
                LineNumber=(-1);
                Indentation=(-1);
            }

        