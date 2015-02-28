namespace IronJade.Util
open System.Text.RegularExpressions
    module ActivePattern=
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