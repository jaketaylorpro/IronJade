namespace IronJade.Util
    module List=
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
        let rec foldWhile<'S,'A> (f:'S->'A->Option<'S>*'A) (state:'S) (acc:'A) :'A=
            let (newState,newAcc) = f state acc
            match newState with
            |None -> newAcc
            |Some(v) -> foldWhile f v newAcc
        let forallpeek<'A> (f:Option<'A> -> 'A -> Option<'A> -> bool) (l:List<'A>) :bool=
            let rec h (last:Option<'A>) (list:List<'A>) =
                match list with
                | [] -> true
                | [a] -> f last a None
                | a::n::r -> (f last a (Some(n))) && (h (Some(a)) (n::r))
            h None l
        let tryfindpeek<'A> (f:Option<'A> -> 'A -> Option<'A> -> bool) (l:List<'A>) :Option<'A>=
            let rec h (last:Option<'A>) (list:List<'A>) =
                match list with
                | [] -> None
                | [a] -> if (f last a None) then  Some(a) else None
                | a::n::r -> if (f last a (Some(n))) then Some(a) else (h (Some(a)) (n::r))
            h None l