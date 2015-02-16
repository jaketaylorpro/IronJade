namespace IronJade.Test

open FsUnit
open NUnit.Framework
open Swensen.Unquote
open IronJade.Lexer
open IronJade.Compiler
open IronJade.Formatter

    module ComprehensiveTests=
        let t s=
            s
            |> lexLines
            |> compileLexNode []
            |> formatLexNode

        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``html only``() =
            test <@ t ["html"] = "<html></html>" @>

