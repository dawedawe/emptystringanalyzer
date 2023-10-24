module EmptyStringAnalyzer.Analyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open EmptyStringAnalyzer.TASTCollecting

let (|EmptyStringConst|_|) (e: FSharpExpr) =
    if e.Type.ErasedType.BasicQualifiedName = "System.String" then
        match e with
        | FSharpExprPatterns.Const(o, _type) when
            not (isNull o)
            && System.String.Empty.Equals(string o, System.StringComparison.Ordinal)
            ->
            Some()
        | _ -> None
    else
        None

let invalidStringFunctionUseAnalyzer
    (typedTree: FSharpImplementationFileContents)
    (typedArgumentPredicate: FSharpExpr list -> bool)
    =
    let ranges = ResizeArray<range>()

    let handler: Handler =
        CallHandler(fun (m: range) (mfv: FSharpMemberOrFunctionOrValue) (args: FSharpExpr list) ->
            match (mfv.Assembly.SimpleName, mfv.FullName, args) with
            | "FSharp.Core", "Microsoft.FSharp.Core.Operators.(=)", [ _; EmptyStringConst ]
            | "FSharp.Core", "Microsoft.FSharp.Core.Operators.(=)", [ EmptyStringConst; _ ] -> ranges.Add m
            | _ -> ())

    for decl in typedTree.Declarations do
        visitDeclaration handler decl

    ranges
    |> Seq.map (fun r ->
        { Type = "EmptyString analyzer"
          Message = "Test for empty strings should use the String.Length property or the String.IsNullOrEmpty method"
          Code = "ES001"
          Severity = Warning
          Range = r
          Fixes = [] })
    |> Seq.toList


[<CliAnalyzer>]
let emptyStringAnalyzer: Analyzer<CliContext> =
    fun (ctx: CliContext) ->
        async {
            match ctx.TypedTree with
            | None -> return List.empty
            | Some typedTree ->
                return
                    invalidStringFunctionUseAnalyzer typedTree (fun (args: FSharpExpr list) ->
                        match args with
                        | [ _; EmptyStringConst ]
                        | [ EmptyStringConst; _ ] -> true
                        | _ -> false)
        }
