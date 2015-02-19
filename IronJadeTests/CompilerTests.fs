namespace IronJade.Test
open FsUnit
open NUnit.Framework
open Swensen.Unquote
open IronJade
open IronJade.Lexer
open IronJade.Compiler
open IronJade.Test.LexerTests

    module CompilerTests=
        [<Test>]
        [<CategoryAttribute("Compiler")>]
        let ``html only no subs``() =
            let env:List<string*string>=[]
            let htmlNode=(buildRootNode Tab [(buildEmptyNode (buildEmptyLine (buildEmptyTag "html")))])
            test <@  (compileLexNode env htmlNode) = htmlNode @>