module Program

open System.Collections.Generic
open System.IO

open FsiX.Features
open FsiX.ProjectLoading
open FsiX.AppState
open PrettyPrompt.Completion


type FsiCallBacks(app: AppState) =
    inherit PrettyPrompt.PromptCallbacks()

    override _.GetCompletionItemsAsync(text, caret, spanToBeReplaced, _) =
        task {
            let typedWord = text.Substring(spanToBeReplaced.Start, spanToBeReplaced.Length)
            return app.GetCompletions(text, caret, typedWord) :> IReadOnlyList<CompletionItem>
        }

let main () =
    task {
        let! app =
            let sln = loadSolution <| Directory.GetCurrentDirectory()
            AppState.mkAppState sln

        let prompt =
            PrettyPrompt.Prompt(
                persistentHistoryFilepath = "./.fsix_history",
                callbacks = FsiCallBacks(app),
                configuration = app.GetPromptConfiguration()
            )

        while true do
            let! response = prompt.ReadLineAsync()

            if response.IsSuccess then
                match response.Text[0] with
                | ':'
                | '#' -> do! DirectiveProcessor.runAnyDirective response.Text app response.CancellationToken
                | _ -> app.EvalCode(response.Text, response.CancellationToken)

    }

main () |> _.GetAwaiter() |> _.GetResult()
