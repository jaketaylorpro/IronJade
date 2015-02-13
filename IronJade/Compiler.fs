namespace IronJade
open LexTag
open LexLine
open LexNode
    module Compiler=
        let compileString (env:List<string*string>) (s:Option<string>) :Option<string>=
            match s with
            |None -> None
            |Some(st) ->
                Some(List.fold (fun acc (n,v) ->st.Replace("#{"+n+"}",v)) st env)
        let compileLexNode (rootNode:LexNode) (environment:List<string*string>) :LexNode=
            let compileStringWithEnv = compileString environment
            let compileTag (tag:LexTag) =
                match tag with
                |LexTag.LexTagError -> LexTag.LexTagError
                |LexTag.LexTagProper(v) -> 
                    let text=compileStringWithEnv v.Text
                    let attrN,attrV=v.Attributes
                                    |>List.unzip
                    let attributes=List.zip attrN (attrV|>List.map compileStringWithEnv)
                    LexTag.LexTagProper(new LexTagValue(v.Name,attributes,text))
            let compileLexLine (line:LexLine) :LexLine=
                match line with
                |LexLine.Root -> LexLine.Root
                |LexLine.TextBlockLine(text) -> LexLine.TextBlockLine((compileStringWithEnv (Some(text))).Value)
                |LexLine.TextLine(text) -> LexLine.TextLine((compileStringWithEnv (Some(text))).Value)
                |LexLine.Tag(tag) -> LexLine.Tag(compileTag tag)
            let rec h (n:LexNode) :LexNode=
                new LexNode(n.LexLine,n.ChildNodes|>List.map h,n.Indentation,n.LineNumber)
            h rootNode