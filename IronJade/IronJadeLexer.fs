module IronJade
open System.Text.RegularExpressions
    module Lexer=
        module Util=
            let takeBeforeAndAfter (f:'a->bool) (l:List<'a>) :List<'a>*List<'a>=
                let rec h (after:bool) (l:List<'a>) (p1:List<'a>) (p2:List<'a>) :List<'a>*List<'a>=
                    match l with
                    | [] -> p1,p2
                    | x::_-> match after,(f x) with
                             | true,_ | false,true -> h true l.Tail p1 (x::p2)
                             | false,false -> h false l.Tail (x::p1) p2
                h false l [] []
            let (|Prefix|_|) (p:string) (s:string) =
                if s.StartsWith(p) then
                    Some(s.Substring(p.Length))
                else
                    None
            let (|Regex|_|) pattern input =
                let m = Regex.Match(input, pattern)
                if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
                else None
        type LexTag(tag:string,attributes:List<string*string>,text:string)=
            member this.Name=tag
            member this.Attributes=attributes
            member this.Text=text
        let buildLexTag (line:string)=
            match line with
            | Util.Regex "([a-zA-Z0-9_]+)(\#[a-zA-Z0-9_])(\.[a-zA-Z0-9_])*" []->
            | _-> //tODO more regex
        type LexLine=
            |Tag of LexTag
            |TextLine of string
            |TextBlockLine of string
            |Root
        let buildLexLine (line:string) :LexLine= //TODO block text not yet implemented
            match line with
            | Util.Prefix "|" rest -> TextLine(rest)
            | _-> Tag(buildLexTag line)
        type LexNode(lexLine:LexLine,childNodes:List<LexNode>,ind:int)=
            member this.LexLine=lexLine
            member this.ChildNodes=childNodes
            member this.Ind=ind
        let LexLines (lines:List<string>)=
            let rec groupLines (lines:List<int*string>) (nodes:List<LexNode>) :List<LexNode>=
                match lines with
                | [] -> //done, no lines left to process
                    nodes
                | (ind1,l1)::_ ->
                    let indentedLines,restLines=lines|>Util.takeBeforeAndAfter (fun o -> match o with
                                                                                    |(i,s) when i>ind1 -> false //we continue taking util indentation is greater than current
                                                                                    | _ -> true)//and the remainder go get parsed at the same level
                    match indentedLines with
                    | []-> groupLines restLines (new LexNode((buildLexLine l1),[],ind1)::nodes)//no indented lines means no children of this node
                    | _ ->groupLines restLines (new LexNode((buildLexLine l1),(groupLines indentedLines []),ind1)::nodes)//some indented lines mean children of the node, and the rest are siblings
            let tabbedLines=lines
                            //calculate indentation
                            |>Seq.map (fun l->(l.ToCharArray()|>Seq.takeWhile (fun c->c='\t'))|>Seq.length,l.TrimStart())
                            |>Seq.toList
            new LexNode(LexLine.Root,(groupLines tabbedLines []),-1)
        let LexFile (path:string)=
            LexLines (System.IO.File.ReadAllLines(path)|>Array.toList)

        