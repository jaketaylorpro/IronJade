namespace IronJade
    module Compiler=
        let compileString (env:List<string*string>) (s:string) :string=
            List.fold (fun (acc:string) (n,v) ->acc.Replace("#{"+n+"}",v)) s env
        let compileLexNode (env:List<string*string>) (rootNode:LexNode.LexNode) :LexNode.LexNode=
            let compileStringWithEnv = compileString env
            let compileOptStringWithEnv text= match text with
                                              |None -> None
                                              |Some(s) -> Some(compileString env s)
            let compileTag (tag:LexTag.LexTag) =
                match tag with
                |LexTag.LexTagError(_)|LexTag.LexTagInnerError(_) -> tag //no text to compile, so we return the same record
                |LexTag.LexTagProper({Name=name;Attributes=attributes;LexInnerTag=innerTag}) ->
                    let attrN,attrV=attributes|>List.unzip
                    let compiledAttributes=List.zip attrN (attrV|>List.map compileOptStringWithEnv)
                    let compiledInnerTag=match innerTag with
                                         |LexInnerTag.BlockText|LexInnerTag.InnerLexTagError(_)|LexInnerTag.Normal -> innerTag //no text to compile, so we return the same record
                                         |LexInnerTag.Inline(text) -> LexInnerTag.Inline(compileStringWithEnv text)
                    LexTag.LexTagProper({Name=name;Attributes=compiledAttributes;LexInnerTag=compiledInnerTag})
            let compileLexLine (line:LexLine.LexLine) :LexLine.LexLine=
                match line with
                |LexLine.Root -> LexLine.Root
                |LexLine.DocType(text) -> LexLine.DocType (compileStringWithEnv text)
                |LexLine.TextBlockLine(text) -> LexLine.TextBlockLine(compileStringWithEnv text)
                |LexLine.TextLine(text) -> LexLine.TextLine(compileStringWithEnv text)
                |LexLine.Tag(tag) -> LexLine.Tag(compileTag tag)
            let rec h (n:LexNode.LexNode) :LexNode.LexNode=
                {LexLine=n.LexLine;ChildNodes=n.ChildNodes|>List.map h;Indentation=n.Indentation;LineNumber=n.LineNumber}
            h rootNode