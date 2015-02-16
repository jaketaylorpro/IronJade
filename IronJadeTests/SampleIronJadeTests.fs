namespace IronJade.Test


// https://github.com/fsharp/FsUnit
// https://code.google.com/p/unquote/

open FsUnit
open NUnit.Framework
open Swensen.Unquote

    module SampleTests=
        [<Test>]
        [<CategoryAttribute("Sample")>]
        let ``FsUnit test 1``() =
            [1; 2; 3] |> should equal [1; 2; 3; 4]

        [<Test>]
        [<CategoryAttribute("Sample")>]
        let ``FsUnit test 2``() =
            1 |> should not' (equal 1)

        [<Test>]
        [<CategoryAttribute("Sample")>]
        let ``FsUnit test 3``() =
            10.1 |> should (equalWithin 0.1) 10.22

        [<Test>]
        [<CategoryAttribute("Sample")>]
        let ``FsUnit test 4 (should throw exception)``() =
            (fun () -> 1 + 2 |> ignore) |> should throw typeof<System.Exception>

        [<Test>]
        [<CategoryAttribute("Sample")>]
        let ``Unquote test 1``() =
            test <@ ([3; 2; 1; 0] |> List.map ((+) 1)) = [1 + 3..1 + 0] @>