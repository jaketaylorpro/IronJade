namespace IronJade.Test
open FsUnit
open NUnit.Framework
open Swensen.Unquote
open IronJade
open IronJade.Lexer
open Newtonsoft.Json
    module LexerTests=
        let buildEmptyNode t=
            {LexLine=t;ChildNodes=[];LineNumber=0;Indentation=0}
        let buildEmptyTag name=
            LexTag.LexTagProper({Name=name;Attributes=[];LexInnerTag=Normal})
        let buildEmptyLine t=
            LexLine.Tag(t)
        let buildRootNode ind c=
            {LexLine=Root(ind,NotCompiled);ChildNodes=c;LineNumber=(-1);Indentation=(-1)}
        [<Test>]
        [<CategoryAttribute("Lexer")>]
        let ``build html`` () =
            test <@  (LexTagBuilder.buildLexTag "html") = (buildEmptyTag "html") @>
            
        [<Test>]
        [<CategoryAttribute("Lexer")>]
        let ``full test html only``() =
            test <@  lexLines ["html"] = (buildRootNode Tab [(buildEmptyNode (buildEmptyLine (buildEmptyTag "html")))]) @>
        
        let htmlAndBody ind=
            buildRootNode ind
                [
                    {
                        LexLine=(buildEmptyLine (buildEmptyTag "html"));
                        ChildNodes=
                            [
                                {
                                    LexLine=(buildEmptyLine (buildEmptyTag "body"));
                                    ChildNodes=[];
                                    LineNumber=1;
                                    Indentation=1
                                }
                            ];
                        LineNumber=0;
                        Indentation=0
                    }
                ]
        [<Test>]
        [<CategoryAttribute("Lexer")>]
        let ``full test tabs``() =
            test <@  lexLines ["html";"\tbody"] = (htmlAndBody Tab) @>
        [<Test>]
        [<CategoryAttribute("Lexer")>]
        let ``full test spaces2`` () =
            test <@  lexLines ["html";"  body"] = (htmlAndBody (Space(2))) @>
        [<Test>]
        [<CategoryAttribute("Lexer")>]
        let ``full test spaces4`` () =
            test <@  lexLines ["html";"    body"] = (htmlAndBody (Space(4))) @>