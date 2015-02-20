namespace IronJade.Test
open FsUnit
open NUnit.Framework
open Swensen.Unquote
open IronJade
open IronJade.Lexer
open IronJade.Compiler
open IronJade.Test.LexerTests

    module CompilerTests=
        let buildCompiledRootNode ind c=
            {LexLine=Root(ind,Compiled);ChildNodes=c;LineNumber=(-1);Indentation=(-1)}
            
        [<Test>]
        [<CategoryAttribute("Compiler")>]
        let ``html only no subs``() =
            let env:List<string*obj>=[]
            let htmlNode=(buildCompiledRootNode Tab [(buildEmptyNode (buildEmptyLine (buildEmptyTag "html")))])
            test <@  (compileLexNode env htmlNode) = htmlNode @>