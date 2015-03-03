namespace IronJade.Util
open System.Text.RegularExpressions
    module ActivePattern=
        let (|Prefix|_|) (p:string) (s:string) =
            if s.StartsWith(p) then
                Some(s.Substring(p.Length))
            else
                None
        let (|Suffix|_|) (p:string) (s:string) =
            if s.EndsWith(p) then
                Some(s.Substring(0,s.Length-p.Length))
            else
                None
        let (|Contains|_|) (p:string) (s:string) =
            if s.Contains(p) then
                Some(s.IndexOf(p))
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