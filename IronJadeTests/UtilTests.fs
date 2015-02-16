﻿namespace IronJade.Test
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
            | Util.Regex noPattern [oneMatch] -> failwith "pattern should not be a match"
            | Util.Regex yesPattern [oneMatch] -> oneMatch|> should equal "bc"
            | _ -> failwith "should match above"
        [<Test>]
        [<CategoryAttribute("Util")>]
        let ``regex one or two match`` () =
            let yesPattern="a(.*)d(f)?"
            let noPattern="a(.*)e"
            let testString="abcd"
            match testString with
            | Util.Regex noPattern [matchOne;matchTwo] -> failwith "pattern should not be a match"
            | Util.Regex yesPattern [matchOne;matchTwo] -> matchOne|> should equal "bc"
                                                           matchTwo|> should equal ""
            | _ -> failwith "should match above"
        [<Test>]
        [<CategoryAttribute("Util")>]
        let ``regex multiple match`` () =
            let yesPattern="a(.*)d([fgh])*"
            let noPattern="a(.*)e"
            let testString="abcdfgh"
            match testString with
            | Util.RegexMC noPattern [[matchOne];[matchTwo]] -> failwith "pattern should not be a match"
            | Util.RegexMC yesPattern [[matchOne];[matchTwoA;matchTwoB]] -> failwith "pattern should not be a match, count of captures of second group was wrong"
            | Util.RegexMC yesPattern [[matchOne];[matchTwoA;matchTwoB;matchTwoC]] -> matchOne|> should equal "bc"
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
            test <@Util.getRegexGroupsByName yesPattern testString = Some(expected)@>
            test <@Util.getRegexGroupsByName noPattern testString = None@>