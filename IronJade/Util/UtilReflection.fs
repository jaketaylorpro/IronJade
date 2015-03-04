namespace IronJade.Util
open Newtonsoft.Json
open System
open System.Dynamic
open System.Linq.Expressions
open System.Reflection
open System.Runtime.CompilerServices
open Microsoft.CSharp.RuntimeBinder
open Microsoft.FSharp.Reflection

open System.Dynamic
    module Reflection=
        type StringBinder(name:string)=
            inherit GetMemberBinder(name,false)
            //override this.ReturnType = typedefof<string>
            override this.FallbackGetMember(target:DynamicMetaObject,errorSuggestion:DynamicMetaObject)=
                failwith "unimplemented dynamic binder method"
        let jobjToKvp (o:Linq.JObject) :List<string*obj>=
            (System.Linq.Enumerable.ToList (o.Properties()))
            |>Seq.map(fun p->p.Name,p.Value.ToObject(typeof<System.Object>))
            |>Seq.toList
        (*let viewBagToKvp (o:DynamicObject) :List<string*obj>=
            let names=o.GetDynamicMemberNames()
            let argInfos=[| CSharpArgumentInfo.Create(CSharpArgumentInfoFlags.None, null) |]
            let kvp=names
                    |>Seq.map (fun k -> let getMemberBinder=Binder.GetMember(CSharpBinderFlags.None,k,null,argInfos)
                                        let v=CallSite<Func<CallSite,obj,obj>>.Create(getMemberBinder).Target.Invoke(cs,)
                                        k,v.ToString():>obj)
                    |>Seq.toList
            kvp*)
        let viewBagToKvp (o:DynamicObject) :List<string*obj>=
            jobjToKvp (JsonConvert.DeserializeObject(JsonConvert.SerializeObject(o)):?>Linq.JObject)
        let objToKvp (o:obj) :List<string*obj>=
            o.GetType().GetProperties()
            |>Array.map (fun pi->pi.Name,pi.GetValue(o))
            |>Array.toList
            