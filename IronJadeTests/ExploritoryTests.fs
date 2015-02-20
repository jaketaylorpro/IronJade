namespace IronJade.Test
open FsUnit
open NUnit.Framework
open Swensen.Unquote
open System.Text.RegularExpressions
open Newtonsoft.Json
    module ExploritoryTests =
        [<Test>]
        [<CategoryAttribute("Exploritory")>]
        let ``equals test`` () :unit=
            test <@ (int32 1L) = (1) @>
            test <@ ((1L,1):>obj) = ((int64 1,1):>obj) @>

