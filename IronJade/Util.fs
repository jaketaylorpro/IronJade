namespace IronJade
open System.Text.RegularExpressions
    module Util=
        let mapfoldi (f:'S->int->'a->'b*'S) (s:'S) (l:List<'a>) :List<'b>*'S=
            let rec h s i l lr=
                match l with
                |[] -> lr,s
                |_ -> let (mv,newS)=f s i (l.Head)
                      h newS (i+1) (l.Tail) (mv::lr)
            h s 0 l []
        let mapfold (f:'S->'a->'b*'S) (s:'S) (l:List<'a>) :List<'b>*'S=
            mapfoldi (fun acc i v -> f acc v) s l
        let mapfoldifst f s l=
            fst (mapfoldi f s l)
        let mapfoldfst f s l=
            fst (mapfold f s l)
        let takeWhileAndRest (f:'a->bool) (l:List<'a>) :List<'a>*List<'a>=
            let groups=l|>Seq.groupBy (fun a -> f a)|>Seq.sortBy fst|>Seq.toList
            match groups with
            | [(true,take)]              -> (take|>Seq.toList),[]
            | [(false,rest)]             -> []                ,(rest|>Seq.toList)
            | [(true,take);(false,rest)] -> (take|>Seq.toList),(rest|>Seq.toList)
            | [] | _                     -> []                ,[] //it's a little ugly to have the default case return blank, but it can't ever be reached when grouping by bool
        let ifSomeTrimmed (s:string)=
            match s.Trim() with
            |"" -> None
            |st -> Some(st)
        let ifNoneEmpty (s:Option<string>)=
            match s with
            |None -> ""
            |Some(st) -> st
        let getRegexGroupsByName p s=
            let r = new Regex(p)
            let m = r.Match(s)
            if m.Success 
            then
                let groupNames=r.GetGroupNames()
                let groups=groupNames
                           |>Array.map (fun gn-> gn,[for c in (m.Groups.[gn].Captures) -> c.Value])
                           |>Map.ofArray
                Some(groups)
            else
                None
        //active patterns
        let (|Prefix|_|) (p:string) (s:string) =
            if s.StartsWith(p) then
                Some(s.Substring(p.Length))
            else
                None
        let (|Regex|_|) pattern input =
            let m = Regex.Match(input, pattern)
            if m.Success 
            then Some([ for g in m.Groups -> g]
                      |>List.tail
                      |>List.map (fun g->g.Value))
            else None
        let (|RegexMC|_|) pattern input =
            let m = Regex.Match(input, pattern)
            if m.Success 
            then Some([ for g in m.Groups -> g]
                      |>List.tail
                      |>List.map (fun g->[ for c in g.Captures ->c.Value]))
            else None

