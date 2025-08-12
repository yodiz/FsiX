module FsiX.ProjectReloading.CodeGeneration

#nowarn "57"

open FsiX.ProjectReloading.SymbolParsing
open System
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis


let rangeFromStart (range: range) =
    let start = Position.mkPos range.Start.Line 0
    Range.mkRange range.FileName start range.End

let rec generateCode'
    (text: ISourceText)
    path
    (oldMap: Map<NamespaceString, SymbolHashing.SymbolWithHash>)
    (newMap: Map<NamespaceString, SymbolHashing.SymbolWithHash>)
    =
    let changedType range typeDefs =
        SymbolExtraction.getTypes path range typeDefs
        |> List.exists (fun typeDef ->
            let path = typeDef.Path.ToString()

            if path.EndsWith "SymbolWithHash" then
                printfn $"path is {path}"
                printfn $"oldSymbolHash is {Map.tryFind path oldMap}"
                printfn $"newSymbolHash is {Map.tryFind path newMap}"

            match Map.tryFind path oldMap, Map.tryFind path newMap with
            | Some oldSymbol, Some newSymbol -> oldSymbol.Hash <> newSymbol.Hash
            | _, _ -> true)

    function
    | SynModuleDecl.Let(range = range)
    | SynModuleDecl.Exception(range = range)
    | SynModuleDecl.ModuleAbbrev(range = range)
    | SynModuleDecl.Attributes(range = range)
    | SynModuleDecl.HashDirective(range = range)
    | SynModuleDecl.Expr(range = range)
    | SynModuleDecl.Open(range = range) -> [ range |> rangeFromStart |> text.GetSubTextFromRange ]
    | SynModuleDecl.Types(range = range; typeDefns = typeDefns) when changedType range typeDefns ->
        [ range |> rangeFromStart |> text.GetSubTextFromRange ]
    | SynModuleDecl.NestedModule(decls = decls; moduleInfo = SynComponentInfo(longId = longId; range = moduleRange)) ->
        match decls with
        | [] -> []
        | x :: _ ->
            let identPre = String.replicate (moduleRange.StartColumn) " "
            let identIn = String.replicate (x.Range.StartColumn) " "
            let nextPath = NamespacePath.addLongId longId path

            let moduleDecl =
                $"{identPre}module {NamespacePath.ofLongId longId |> _.ToString()} =\n"

            let moduleOpen = $"{identIn}open {nextPath.ToString()}\n"

            identPre
            :: moduleDecl
            :: identIn
            :: moduleOpen
            :: List.collect (generateCode' text nextPath oldMap newMap) decls
    | SynModuleDecl.NamespaceFragment(_) -> [] //idk what's that
    | SynModuleDecl.Types(_) -> [] //dont include type defs if they have not been changed

let generateCode source (parseResults: FSharpParseFileResults) oldMap newMap =
    let (ParsedInput.ImplFile(ParsedImplFileInput(contents = contents))) =
        parseResults.ParseTree

    let [ SynModuleOrNamespace(decls = codeLines; longId = moduleLong) ] = contents
    let modulePath = NamespacePath.ofLongId moduleLong
    let moduleDecl = $"module {modulePath.ToString()}"
    let moduleOpen = $"open {modulePath.ToString()}"

    moduleDecl
    :: moduleOpen
    :: List.collect (generateCode' source modulePath oldMap newMap) codeLines
    |> fun x -> String.Join("\n", x)

#warn "57"
