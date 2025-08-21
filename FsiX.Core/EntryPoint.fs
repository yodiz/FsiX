module FsiX.EntryPoint

open System.Collections.Generic
open System.IO

open FsiX.Features
open FsiX.ProjectLoading
open FsiX.AppState
open PrettyPrompt.Completion


type FsiCallBacks(app: AppState) =
    inherit PrettyPrompt.PromptCallbacks()

    override _.ShouldOpenCompletionWindowAsync (text: string, caret: int, keyPress: PrettyPrompt.Consoles.KeyPress, cancellationToken: System.Threading.CancellationToken): System.Threading.Tasks.Task<bool> = task { return true }
    override _.GetCompletionItemsAsync(text, caret, spanToBeReplaced, _) =
        task {
            let typedWord = text.Substring(spanToBeReplaced.Start, spanToBeReplaced.Length)
            return app.GetCompletions(text, caret, typedWord) :> IReadOnlyList<CompletionItem>
        }

let main useAllRefs () =
    task {
        let! app =
            let sln = loadSolution <| Directory.GetCurrentDirectory()
            AppState.mkAppState useAllRefs sln

        let prompt =
            PrettyPrompt.Prompt(
                persistentHistoryFilepath = "./.fsix_history",
                callbacks = FsiCallBacks app,
                configuration = app.GetPromptConfiguration()
            )
        app.OutStream.Enable()

        while true do
            try
                
                let! response = prompt.ReadLineAsync()
                if response.IsSuccess && response.Text <> "" then
                    match response.Text[0] with
                    | ':'
                    | '#' -> do! DirectiveProcessor.runAnyDirective response.Text app response.CancellationToken
                    | _ ->
                        let! code = ComputationExpressionSimplifier.rewriteCompExpr response.Text
                        app.EvalCode(code, response.CancellationToken)
            with _ -> ()

    }

