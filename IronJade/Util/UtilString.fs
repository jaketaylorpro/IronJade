namespace IronJade.Util
open System.Text.RegularExpressions
    module String=
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
        let ifSomeTrimmed (s:string)=
            match s.Trim() with
            |"" -> None
            |st -> Some(st)
        let ifNoneEmpty (s:Option<string>)=
            match s with
            |None -> ""
            |Some(st) -> st
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
        let removeAllNewlines (s:string) :string=
            Regex.Replace(s,"\n","")
        let htmlTrim (s:string) :string=
            let s1=Regex.Replace(s,">\s+([^\s])",">$1")
            Regex.Replace(s1,"([^\s])\s+<","$1<")
        let htmlCommentTrim (s:string) :string=
            let s1=Regex.Replace(s,"<!--\s+","<!--")
            Regex.Replace(s1,"\s+-->","-->")