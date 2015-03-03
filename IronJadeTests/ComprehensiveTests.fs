namespace IronJade.Test

open FsUnit
open NUnit.Framework
open Swensen.Unquote
open IronJade.Lexer
open IronJade
open IronJade.Compiler
open IronJade.Formatter
open Newtonsoft.Json

    module ComprehensiveTests=
        let t s e=
            let w=new System.IO.StringWriter()
            let n= s|> lexLines|> compileLexNode e
            ignore(formatLexNode n (ref (w:>System.IO.TextWriter)))
            w.ToString()
            |>Util.String.removeWhitespace
            |>Util.String.removeAllNewlines
            |>Util.String.htmlTrim
            |>Util.String.htmlCommentTrim
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``html only``() =
            test <@ t ["html"] [] = "<html></html>" @>
        
        let comprehensiveTester (testName:string)=
            let jadeFilePath=System.String.Format("../../tests/{0}.jade",testName)
            let jsonFilePath=System.String.Format("../../tests/{0}.json",testName)
            let htmlFilePath=System.String.Format("../../tests/{0}.html",testName)
            let jadeLines=System.IO.File.ReadAllLines(jadeFilePath)
            let htmlContents=System.IO.File.ReadAllLines(htmlFilePath)
                             |>Util.String.joinLines//TODO we want it to look like this eventually without the two proceding steps
                             |>Util.String.removeWhitespace
                             |>Util.String.removeAllNewlines
                             |>Util.String.htmlTrim
                             |>Util.String.htmlCommentTrim
            
            let env=if System.IO.File.Exists(jsonFilePath)
                    then
                        JsonConvert.DeserializeObject(System.IO.File.ReadAllLines(jsonFilePath)|>Util.String.joinLines):?>Linq.JObject
                        |>Util.Reflection.jobjToKvp
                    else
                        []
            test <@ t jadeLines env= htmlContents @>
            
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``demo_0``() =
            comprehensiveTester "demo_0"
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``demo_1``() =
            comprehensiveTester "demo_1"
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``demo_2``() =
            comprehensiveTester "demo_2"
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``pipped_text``() =
            comprehensiveTester "pipped_text"
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``comments``() =
            comprehensiveTester "comments"
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``hidden comments``() =
            comprehensiveTester "comments_hidden"
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``block comments``() =
            comprehensiveTester "block_comments"
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``hidden block comments``() =
            comprehensiveTester "block_comments_hidden"
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``self closing img``() =
            comprehensiveTester "self_closing_img"
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``self closing tags``() =
            comprehensiveTester "self_closing_tags"
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``inline tags``() =
            comprehensiveTester "inline_tags"
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``inline tag``() =
            comprehensiveTester "inline_tag"
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``block text tab``() =
            comprehensiveTester "block_text_tab"
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``block text space 2``() =
            comprehensiveTester "block_text_space_2"
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``block text space 4``() =
            comprehensiveTester "block_text_space_4"


