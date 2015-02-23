namespace IronJade.Test
open FsUnit
open NUnit.Framework
open Swensen.Unquote
open System.Text.RegularExpressions
open Newtonsoft.Json
    module ExploritoryTests =
        [<Test>]
        [<CategoryAttribute("Exploratory")>]
        let ``equals test`` () :unit=
            test <@ (int32 1L) = (1) @>
            test <@ ((1L,1):>obj) = ((int64 1,1):>obj) @>

        [<Test>]
        [<CategoryAttribute("Exploratory")>]
        let ``id pattern test1`` () :unit=
            test <@ Regex.IsMatch("#tableid","^\#(?<id>[a-zA-Z][a-zA-Z0-9_-]*)$") @>
        [<Test>]
        [<CategoryAttribute("Exploratory")>]
        let ``id pattern test2`` () :unit=
            test <@ Regex.IsMatch("#tableid","^\#(?<id>[a-zA-Z][a-zA-Z0-9_-]*)$") @>
        [<Test>]
        [<CategoryAttribute("Exploratory")>]
        let ``id pattern test simplified`` () :unit=
            test <@ Regex.IsMatch("#tableid","^\#[a-zA-Z][a-zA-Z0-9_-]*$") @>