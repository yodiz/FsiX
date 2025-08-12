module FsiX.AppState

open System.IO

open System.Threading
open FSharp.Compiler.Interactive.Shell
open FSharpPlus
open FsiX.Features
open FsiX.ProjectLoading

open FsiX.ProjectReloading.FileWatcher
open PrettyPrompt

type private ReloadingState =
    { Agent: MailboxProcessor<Agent.Message>
      FsWatcher: FileSystemWatcher }

type FilePath = string

type AppState private (sln: Solution, reloading, session: FsiEvaluationSession, globalConfig, localConfig) =
    member _.Solution = sln
    member _.InteractiveChecker = session.InteractiveChecker

    member _.EvalCode(code, token) =
        try
            session.EvalInteraction(code, token)
        with _ ->
            ()

    member this.GetPromptConfiguration() =
        this.EvalCode(globalConfig, CancellationToken.None)

        match localConfig with
        | None -> ()
        | Some localConfig -> this.EvalCode(localConfig, CancellationToken.None)

        let PromptConfigurationValue =
            session.GetBoundValues()
            |> List.tryFind (fun x -> x.Value.ReflectionType.Name = nameof PromptConfiguration)

        match PromptConfigurationValue with
        | None -> failwith "No PromptConfiguration was found!"
        | Some v -> (v.Value.ReflectionValue :?> PromptConfiguration)

    member _.Reload(fileToReloadOpt, token: CancellationToken) =
        async {
            let! result =
                reloading.Agent.PostAndAsyncReply(fun this -> Agent.Reload(session.EvalScript, fileToReloadOpt, this))

            match result with
            | Ok() -> printfn "Reloaded!"
            | Error ex -> printfn $"Error! {ex}"
        }
        |> fun s -> Async.StartAsTask(s, cancellationToken = token)

    member _.GetCompletions(text, caret, word) =
        AutoCompletion.getCompletions session text caret word

    static member mkAppState sln =
        task {
            let globalConfigTask = Configuration.loadGlobalConfig ()
            let localConfigTask = Configuration.loadLocalConfig ()
            let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
            let args = solutionToFsiArgs sln

            let fsiSession =
                FsiEvaluationSession.Create(
                    fsiConfig,
                    args,
                    new StreamReader(Stream.Null),
                    stdout,
                    stdout,
                    collectible = true
                )

            let checker = fsiSession.InteractiveChecker

            let reloadState =
                let agent = Agent.makeAgent checker sln
                let watch = Agent.makeWatcher agent sln
                { Agent = agent; FsWatcher = watch }

            let! globalConfig, localConfig = Task.map2 tuple2 globalConfigTask localConfigTask
            return AppState(sln, reloadState, fsiSession, globalConfig, localConfig)
        }
