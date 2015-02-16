namespace IronJade.Test
open FsUnit
open NUnit.Framework
open Swensen.Unquote
open IronJade.Util
open IronJade.LexInnerTag
open IronJade.LexTag
open IronJade.LexLine
open IronJade.LexNode
open IronJade.Lexer
open Newtonsoft.Json
    module LexerTests=
        let buildEmptyNode t=
            {LexLine=t;ChildNodes=[];LineNumber=0;Indentation=0}
        let buildEmptyTag s=
            LexTag.LexTagProper({Name="html";Attributes=[];LexInnerTag=Normal})
        let buildEmptyLine t=
            LexLine.Tag(t)
        let buildRootNode c=
            {LexLine=Root;ChildNodes=c;LineNumber=(-1);Indentation=(-1)}
        [<Test>]
        [<CategoryAttribute("Lexer")>]
        let ``build html`` () =
            test <@  (buildLexTag "html") = (buildEmptyTag "html") @>

        [<Test>]
        [<CategoryAttribute("Lexer")>]
        let ``full test html only``() =
            test <@  lexLines ["html"] = (buildRootNode [(buildEmptyNode (buildEmptyLine (buildEmptyTag "html")))]) @>