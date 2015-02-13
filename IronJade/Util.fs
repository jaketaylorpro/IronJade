namespace IronJade
open System.Text.RegularExpressions
    module Util=
        let takeBeforeAndAfter (f:'a->bool) (l:List<'a>) :List<'a>*List<'a>=
            let rec h (after:bool) (l:List<'a>) (p1:List<'a>) (p2:List<'a>) :List<'a>*List<'a>=
                match l with
                | [] -> p1,p2
                | x::_-> match after,(f x) with
                            | true,_ | false,true -> h true l.Tail p1 (x::p2)
                            | false,false -> h false l.Tail (x::p1) p2
            h false l [] []
        let ifSomeTrimmed (s:string)=
            match s.Trim() with
            |"" -> None
            |st -> Some(st)
        let ifNoneEmpty (s:Option<string>)=
            match s with
            |None -> ""
            |Some(st) -> st
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

