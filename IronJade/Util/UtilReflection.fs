namespace IronJade.Util
open Newtonsoft.Json
    module Reflection=
        let jobjToKvp (o:Linq.JObject) :List<string*obj>=
            (System.Linq.Enumerable.ToList (o.Properties()))
            |>Seq.map(fun p->p.Name,p.Value.ToObject(typeof<System.Object>))
            |>Seq.toList
        let objToKvp (o:obj) :List<string*obj>=
            let t=o.GetType()
            let props=t.GetFields()
            props
            |>Array.map (fun pi->pi.Name,pi.GetValue(o))
            |>Array.toList
            (*let props=o.GetType().GetProperties()
            props
            |>Array.map (fun pi->pi.Name,pi.GetMethod.Invoke(o,[||]))
            |>Array.toList*)