module EmptyStringAnalyzer.Tests

open NUnit.Framework
open EmptyStringAnalyzer.Analyzer
open FSharp.Analyzers.SDK.Testing

open FSharp.Compiler.CodeAnalysis

let mutable projectOptions: FSharpProjectOptions = FSharpProjectOptions.zero

[<SetUp>]
let Setup () =
    task {
        let! opts = mkOptionsFromProject "net7.0" []

        projectOptions <- opts
    }

[<Test>]
let ``Operator based test for zero-length`` () =
    async {
        let source =
            """
module M

let s = "foo"
let x = s = ""
    """

        let ctx = getContext projectOptions source
        let! msgs = emptyStringAnalyzer ctx
        Assert.IsNotEmpty msgs

        Assert.IsTrue(
            Assert.messageContains
                "Test for empty strings should use the String.Length property or the String.IsNullOrEmpty method"
                msgs[0]
        )
    }

[<Test>]
let ``Operator based test for zero-length reversed`` () =
    async {
        let source =
            """
module M

let s = "foo"
let x = "" = s
    """

        let ctx = getContext projectOptions source
        let! msgs = emptyStringAnalyzer ctx
        Assert.IsNotEmpty msgs

        Assert.IsTrue(
            Assert.messageContains
                "Test for empty strings should use the String.Length property or the String.IsNullOrEmpty method"
                msgs[0]
        )
    }

[<Test>]
let ``Operator based equality test`` () =
    async {
        let source =
            """
module M

let s = "foo"
let x = s = "bar"
    """

        let ctx = getContext projectOptions source
        let! msgs = emptyStringAnalyzer ctx
        Assert.IsEmpty msgs
    }

[<Test>]
let ``Operator based equality test reversed`` () =
    async {
        let source =
            """
module M

let s = "foo"
let x = "bar" = s
    """

        let ctx = getContext projectOptions source
        let! msgs = emptyStringAnalyzer ctx
        Assert.IsEmpty msgs
    }

[<Test>]
let ``Operator based null test`` () =
    async {
        let source =
            """
module M

let s = "foo"
let x = s = null
    """

        let ctx = getContext projectOptions source
        let! msgs = emptyStringAnalyzer ctx
        Assert.IsEmpty msgs
    }

[<Test>]
let ``Operator based null test reversed`` () =
    async {
        let source =
            """
module M

let s = "foo"
let x = null = s
    """

        let ctx = getContext projectOptions source
        let! msgs = emptyStringAnalyzer ctx
        Assert.IsEmpty msgs
    }
