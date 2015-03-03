namespace IronJade.Test
open FsUnit
open NUnit.Framework
open Swensen.Unquote
open System.Text.RegularExpressions
open Newtonsoft.Json
open IronJade
    module UtilTests=
        [<Test>]
        [<CategoryAttribute("Util")>]
        let ``regex one match`` () =
            let yesPattern="a(.*)d"
            let noPattern="a(.*)e"
            let testString="abcd"
            match testString with
            | Util.ActivePattern.Regex noPattern [oneMatch] 
                -> failwith "pattern should not be a match"
            | Util.ActivePattern.Regex yesPattern [oneMatch] 
                -> oneMatch|> should equal "bc"
            | _ -> failwith "should match above"
        [<Test>]
        [<CategoryAttribute("Util")>]
        let ``regex one or two match`` () =
            let yesPattern="a(.*)d(f)?"
            let noPattern="a(.*)e"
            let testString="abcd"
            match testString with
            | Util.ActivePattern.Regex noPattern [matchOne;matchTwo]
                -> failwith "pattern should not be a match"
            | Util.ActivePattern.Regex yesPattern [matchOne;matchTwo] 
                -> matchOne|> should equal "bc"
                   matchTwo|> should equal ""
            | _ -> failwith "should match above"
        [<Test>]
        [<CategoryAttribute("Util")>]
        let ``regex multiple match`` () =
            let yesPattern="a(.*)d([fgh])*"
            let noPattern="a(.*)e"
            let testString="abcdfgh"
            match testString with
            | Util.ActivePattern.RegexMC noPattern [[matchOne];[matchTwo]]
                -> failwith "pattern should not be a match"
            | Util.ActivePattern.RegexMC yesPattern [[matchOne];[matchTwoA;matchTwoB]]
                -> failwith "pattern should not be a match, count of captures of second group was wrong"
            | Util.ActivePattern.RegexMC yesPattern [[matchOne];[matchTwoA;matchTwoB;matchTwoC]]
                -> matchOne |> should equal "bc"
                   matchTwoA|> should equal "f"
                   matchTwoB|> should equal "g"
                   matchTwoC|> should equal "h"
            | _ -> failwith "should match above"
        [<Test>]
        [<CategoryAttribute("Util")>]
        let ``regex multiple named match`` () :unit=
            let yesPattern="(?<start>0)*a(?<mid>.*)d(?<end>[fgh])*"
            let noPattern="a(.*)e"
            let testString="abcdfgh"
            let expected = ["0",[testString];"start",[];"mid",["bc"];"end",["f";"g";"h"]]|>Map.ofList
            test <@Util.String.getRegexGroupsByName yesPattern testString = Some(expected)@>
            test <@Util.String.getRegexGroupsByName noPattern testString = None@>
        [<Test>]
        [<CategoryAttribute("Util")>]
        let ``takewhile test`` () :unit=
            let full=[1;2;3;10;1;2;3]
            let f = (fun i->i<10)
            let take=[1;2;3]
            let rest=[10;1;2;3]
            test <@(Util.List.takeWhileAndRest f full) = (take,rest)@>

        [<Test>]
        [<CategoryAttribute("Util")>]
        let ``objToKvp test`` () :unit=
            let o:Linq.JObject=JsonConvert.DeserializeObject("{i:0,a:'test'}"):?>Linq.JObject
            let kvp:List<string*obj>=["i",0L:>obj;"a","test":>obj]//we need to explicity call i a long because that is the default integer type for json.net
            test <@(Util.Reflection.jobjToKvp o) = kvp@>
        
        
        [<Test>]
        [<CategoryAttribute("Util")>]
        let ``foldWhile test`` () :unit=
            test <@ Util.List.foldWhile (fun i l->if i=0 then (None,l) else (Some(i-1),i::l)) 4 [] = [1;2;3;4] @>

        [<Test>]
        [<CategoryAttribute("Util")>]
        let ``forallpeek test`` () :unit=
            let fib=[1;2;3;5;8;13;21]
            let notfib=[0;1;2;3;5;8;13;21]
            let fibTest=fun (p:Option<int>) (c:int) (n:Option<int>) 
                            -> if p.IsNone || n.IsNone 
                               then true
                               else p.Value + c = n.Value
            test <@Util.List.forallpeek fibTest fib = true@>
            test <@Util.List.forallpeek fibTest notfib = false@>
        [<Test>]
        [<CategoryAttribute("Util")>]
        let ``tryfindpeek test`` () :unit=
            let partfib=[55;82;12;18;17;35;47;106]
            let nopartfib=[55;82;12;18;17;47;106]
            let findFibTest=fun (p:Option<int>) (c:int) (n:Option<int>)
                                -> if p.IsNone || n.IsNone
                                   then false
                                   else p.Value + c = n.Value
            test <@Util.List.tryfindpeek findFibTest partfib = Some(17)@>
            test <@Util.List.tryfindpeek findFibTest nopartfib = None@>
        [<Test>]
        [<CategoryAttribute("Util")>]
        let ``prefix test`` () :unit=
            match "testabc" with
            | Util.ActivePattern.Prefix "test" rest -> test <@rest = "abc"@>
            | _ -> failwith "active pattern did not match when it should have"
            match "testabc" with
            | Util.ActivePattern.Prefix "testb" rest -> failwith "active pattern matched when it should not have"
            | _ -> ()
        [<Test>]
        [<CategoryAttribute("Util")>]
        let ``suffix test`` () :unit=
            match "testabc" with
            | Util.ActivePattern.Suffix "abc" rest -> test <@rest = "test"@>
            | _ -> failwith "active pattern did not match when it should have"
            match "testabc" with
            | Util.ActivePattern.Suffix "btest" rest -> failwith "active pattern matched when it should not have"
            | _ -> ()
        [<Test>]
        [<CategoryAttribute("Util")>]
        let ``contains test`` () :unit=
            match "testabc" with
            | Util.ActivePattern.Contains "abc" i -> test <@i = 4@>
            | _ -> failwith "active pattern did not match when it should have"
            match "testabc" with
            | Util.ActivePattern.Contains "btest" i -> failwith "active pattern matched when it should not have"
            | _ -> ()