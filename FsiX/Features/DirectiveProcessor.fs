module FsiX.Features.DirectiveProcessor

#nowarn "57"

open System.IO
open FSharpPlus
open Fantomas.Core
open Fantomas.FCS.Syntax
open FsiX.AppState
open FsiX.ProjectReloading.SymbolParsing

let runOpenDirective fileToOpen (app: AppState) token = task {
    let fileToOpen = Path.GetFullPath fileToOpen
    let! file = File.ReadAllTextAsync fileToOpen

    let! [|res, _|] = CodeFormatter.ParseAsync(false, file)

    let (ParsedInput.ImplFile(ParsedImplFileInput(contents = contents))) = res

    let [ SynModuleOrNamespace(decls = codeLines; longId = l) ] = contents

    let runOpen (l: LongIdent) =
        let path = NamespacePath.ofLongIdFantomas l |> _.ToString()
        app.EvalCode($"open {path}", token)

    runOpen l

    for codeLine in codeLines do
        match codeLine with
        | SynModuleDecl.Open(target = target) ->
            match target with
            | SynOpenDeclTarget.ModuleOrNamespace(longId = l) -> runOpen l.LongIdent
            | SynOpenDeclTarget.Type(typeName = t) -> () //todo
        | _ -> ()
}

let runReloadDirective commandStrWords (app: AppState) token =
    let fileToReload =
        match commandStrWords with
        | [| _; fileName |] -> Some fileName
        | _ -> None

    app.Reload(fileToReload, token)

let runAnyDirective (commandStr: string) (app: AppState) token =
    let commandStr = "#" + commandStr.Substring 1
    let commandStrWords = commandStr.Split " "

    match commandStrWords[0] with
    | "#reload"
    | "#r" -> runReloadDirective commandStrWords app token
    | "#open"
    | "#o" -> runOpenDirective commandStrWords[1] app token
    | _ -> app.EvalCode(commandStr, token) |> konst Task.result ()

#warn "57"
