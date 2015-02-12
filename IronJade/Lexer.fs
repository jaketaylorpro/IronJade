namespace IronJade
open IronJade.Util
open IronJade.LexTag
open IronJade.LexLine
open IronJade.LexNode
    module Lexer=
        let LexLines (lines:List<string>)=
            let rec groupLines (lines:List<int*int*string>) (nodes:List<LexNode>) :List<LexNode>=
                match lines with
                | [] -> //done, no lines left to process
                    nodes
                | (ln,ind,l)::_ ->
                    let indentedLines,restLines=lines|>Util.takeBeforeAndAfter (fun o -> match o with
                                                                                         |(_,i,_) when i>ind -> false //we continue taking util indentation is greater than current
                                                                                         | _ -> true)//and the remainder go get parsed at the same level
                    match indentedLines with
                    | []-> groupLines restLines (new LexNode((buildLexLine l),[],ln,ind)::nodes)//no indented lines means no children of this node
                    | _ ->groupLines restLines (new LexNode((buildLexLine l),(groupLines indentedLines []),ln,ind)::nodes)//some indented lines mean children of the node, and the rest are siblings
            let tabbedLines=lines
                            //calculate indentation
                            |>Seq.mapi (fun i l->i,(l.ToCharArray()|>Seq.takeWhile (fun c->c='\t'))|>Seq.length,l.TrimStart())
                            |>Seq.toList
            new LexNode(LexLine.Root,(groupLines tabbedLines []),-1,-1)
        let LexFile (path:string)=
            LexLines (System.IO.File.ReadAllLines(path)|>Array.toList)

        