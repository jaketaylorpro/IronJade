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
            let take=Seq.takeWhile f l|>Seq.toList
            let skip=Seq.skipWhile f l|>Seq.toList
            take,skip
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
        let joinLines (l:seq<string>) :string=
            let value=l|>Seq.fold (fun acc v-> match acc with
                                             |None -> Some(System.String.Format("{0}",v))
                                             |Some(s) -> Some(System.String.Format("{0}\n{1}",s,v))) None
            match value with
            |None -> ""
            |Some(s) -> s
        let eraseLines (l:seq<string>) :string=
            l|>Seq.fold (fun acc v-> System.String.Format("{0}{1}",acc,v)) ""
        let removeWhitespace (s:string) :string=
            Regex.Replace(s.Trim(),">\s+<","><")
        let objToKvp o=
            o.GetType().GetProperties()
            |>Array.map (fun pi->pi.Name,pi.GetMethod.Invoke(o,[||]))
            |>Array.collect (fun (k,v) -> match v with
                                          | null -> [||]
                                          | :? string as s -> [|k,s|]
                                          | _ -> [||])
            |>Array.toList
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

