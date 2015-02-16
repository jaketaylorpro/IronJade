namespace IronJade.Test
open FsUnit
open NUnit.Framework
open Swensen.Unquote
open IronJade.LexInnerTag
open IronJade.LexTag
open IronJade.LexLine
open IronJade.LexNode
open IronJade.Lexer
open IronJade.Compiler
open IronJade.Test.LexerTests

    module CompilerTests=
        [<Test>]
        [<CategoryAttribute("Compiler")>]
        let ``html only no subs``() =
            let env:List<string*string>=[]
            let htmlNode=(buildRootNode [(buildEmptyNode (buildEmptyLine (buildEmptyTag "html")))])
            test <@  (compileLexNode env htmlNode) = htmlNode @>