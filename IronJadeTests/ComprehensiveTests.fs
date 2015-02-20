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
            s
            |> lexLines
            |> compileLexNode e
            |> formatLexNode
            
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
                             |>Util.joinLines//TODO we want it to look like this eventually
                             |>Util.removeWhitespace
            
            let env=if System.IO.File.Exists(jsonFilePath)
                    then
                        JsonConvert.DeserializeObject(System.IO.File.ReadAllLines(jsonFilePath)|>Util.joinLines):?>Linq.JObject
                        |>Util.jobjToKvp
                    else
                        []
            test <@ t jadeLines env= htmlContents @>
            
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``demo_0 code``() =
            comprehensiveTester "demo_0"
        [<Test>]
        [<CategoryAttribute("Comprehensive")>]
        let ``demo_1 code``() =
            comprehensiveTester "demo_1"


