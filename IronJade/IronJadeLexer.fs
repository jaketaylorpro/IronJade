module IronJade
    module Lexer=
        type LexNode(line:string,childNodes:List<LexNode>,ind:int)=
            member this.Line=line
            member this.ChildNodes=childNodes
            member this.Ind=ind
            member this.AddChild (node:LexNode) :LexNode=
                let rec reconstruct (node:LexNode) (tails:List<LexNode>)=
                    match tails with 
                    | [] -> node
                    | _ -> 
                            let h,t=tails.Head,tails.Tail
                            reconstruct (new LexNode(h.Line,node::h.ChildNodes,h.Ind)) t
                let rec consNDeep (c:LexNode) (v:LexNode) (n:int) (tails:List<LexNode>) =
                    match n with
                    |1 -> reconstruct (new LexNode(c.Line,v::c.ChildNodes,c.Ind)) tails
                    |_ -> let cnh,cnt=c.ChildNodes.Head,c.ChildNodes.Tail
                          consNDeep cnh v (n-1) (new LexNode(c.Line,cnt,c.Ind)::tails)
                consNDeep this node (node.Ind-this.Ind) []
        let LexLines (lines:seq<string>)=
            lines
            //calculate indentation
            |>Seq.map (fun l->(l.ToCharArray()|>Seq.takeWhile (fun c->c='\t'))|>Seq.length,l.TrimStart())
            //fold over building nodes by indentation
            |>Seq.fold (fun (rootNode:LexNode) ((ind:int),(l:string)) -> 
                            rootNode.AddChild (new LexNode(l,[],ind)))
                        (new LexNode("#",[],-1))
        let LexFile (path:string)=
            LexLines (System.IO.File.ReadLines(path))

        