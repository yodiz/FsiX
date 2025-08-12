module FsiX.ProjectReloading.FileWatcher

open FSharpPlus

open System.IO
open FSharp.Compiler.CodeAnalysis
open FsiX
open FsiX.ProjectReloading.SymbolParsing


#nowarn "57"

module ChangeAnalysis =


    type FileAction =
        | NoAction
        | ReloadCompletely // if file has any type or exn change, we need to reload it fully
        | ReloadChangedOnly

    type FileName = string

    let rec getAllDepsFor (filesWithDeps: Map<FileName, FileName list>) file =
        let deps = filesWithDeps[file]
        file :: List.collect (getAllDepsFor filesWithDeps) deps
    // if this file was changed, what other files need to be updated?
    let rec reverseDepsFor (filesWithDeps: Map<FileName, FileName array>) file =
        let depsForThisFile =
            filesWithDeps
            |> Map.toSeq
            |> Seq.filter (snd >> Array.contains file)
            |> Seq.map fst
            |> List.ofSeq

        file :: List.collect (reverseDepsFor filesWithDeps) depsForThisFile

    //for each file * action check it's deps with actions too
    //all deps after ReloadCompletely also have to be ReloadCompletely
    //group actions by filenames
    //if any action is ReloadCompletely, ReloadCompletely this file
    let fileActionForAll (filesWithDeps: Map<FileName, FileName array>) (fileActions: Map<FileName, FileAction>) =
        let mkFileDepsActionsPairs fileName =
            let reverseDeps = reverseDepsFor filesWithDeps fileName

            let nextFileAction prev this =
                match prev, this with
                | ReloadCompletely, _ -> ReloadCompletely
                | _, t -> t

            let foldMapFn prev this =
                let thisFileAction = Map.tryFind this fileActions |> Option.defaultValue NoAction
                let fileAction = nextFileAction prev thisFileAction
                let nextState = fileAction
                let mapRes = this, fileAction
                mapRes, nextState

            List.mapFold foldMapFn NoAction reverseDeps |> fst

        filesWithDeps
        |> Map.keys
        |> Seq.toList
        |> List.collect mkFileDepsActionsPairs
        |> List.groupBy (fun (fileName, _) -> fileName)
        |> List.map (fun (fileName, actions) ->
            if Seq.exists (snd >> (=) ReloadCompletely) actions then
                fileName, ReloadCompletely
            else
                Seq.head actions)
        |> Map.ofSeq

    let getFileAction (oldFile: FileParseResults) (newFile: FileParseResults) =
        let modifiedSymbols =
            newFile.Symbols
            |> Map.filter (fun sName symbol ->
                Map.tryFind sName oldFile.Symbols |>> (fun oldS -> oldS.Hash <> symbol.Hash)
                |> Option.defaultValue false)

        let typeOrExnWasModified =
            modifiedSymbols
            |> Map.exists (fun _ s -> s.Symbol.SymbolType <> SymbolExtraction.SymbolType.LetDef)

        if typeOrExnWasModified then
            ReloadCompletely
        else
            ReloadChangedOnly

    let computeReloadActions (oldFileMap: Map<FileName, FileParseResults>) newFileMap fileToReload =
        let allFilesMap = Map.union newFileMap oldFileMap

        let fileActionMap =
            Map.zip oldFileMap newFileMap |> Map.mapValues (uncurry getFileAction)

        let fileDepsMap = allFilesMap |> Map.mapValues _.DependencyFiles

        let allFilesToReload =
            getAllDepsFor fileDepsMap fileToReload |> List.rev |> List.distinct

        let fileActions =
            let nextFileAction prev this =
                match prev, this with
                | ReloadCompletely, _ -> ReloadCompletely
                | _, t -> t

            let foldMapFn prev this =
                let thisFileAction = Map.tryFind this fileActionMap |> Option.defaultValue NoAction
                let fileAction = nextFileAction prev thisFileAction
                let nextState = fileAction
                let mapRes = this, fileAction
                mapRes, nextState

            List.mapFold foldMapFn NoAction allFilesToReload |> fst

        allFilesMap, fileActions

module Agent =
    open ChangeAnalysis

    type EvalScriptAction = FileName -> unit

    type Message =
        | FileChanged of string
        | Reload of EvalScriptAction * FileName option * AsyncReplyChannel<Result<unit, exn>>

    let makeAgent fsChecker (sln: ProjectLoading.Solution) =
        MailboxProcessor.Start(fun inbox ->
            let rec messageLoop (sln: ProjectLoading.Solution) symbols changedFiles =
                async {
                    let! msg = inbox.Receive()

                    match msg with
                    | FileChanged fileName when
                        sln.Files.ContainsKey fileName && not (Set.contains fileName changedFiles)
                        ->
                        //printfn "file changed! %s" fileName
                        return! messageLoop sln symbols (Set.add fileName changedFiles)
                    | Reload(evalScript, fileToReload, reply) ->
                        try
                            let sourceOrderedFiles = ProjectLoading.getSourceOrderedFiles sln

                            let topFileToReload, topProjectToReload =
                                match fileToReload with
                                | Some file -> sourceOrderedFiles |> Seq.find (fst >> String.endsWith file)
                                | Option.None -> sourceOrderedFiles |> Seq.last

                            let filesWithProjectsToReload =
                                sourceOrderedFiles
                                |> List.skipWhile (fun (file, _) -> not <| Set.contains file changedFiles)
                                |> List.takeWhile (fun (file, prj) ->
                                    topFileToReload <> file && topProjectToReload <> prj)
                                |> List.groupBy snd

                            let! changedFileDataMap =
                                filesWithProjectsToReload
                                |> Seq.map (fun (prj, changedFiles) ->
                                    async {
                                        let! snapshot =
                                            FSharpProjectSnapshot.FromOptions(prj, DocumentSource.FileSystem)

                                        let changedFiles = changedFiles |> List.map fst

                                        let! rawData =
                                            FileBasedExtraction.getFileDataMap snapshot fsChecker changedFiles

                                        return rawData
                                    })
                                |> Async.Parallel
                                |>> Seq.reduce Map.union

                            printfn "reloading %i files" changedFileDataMap.Count

                            let changedFileMap = FileBasedExtraction.getSymbolsForFiles changedFileDataMap

                            let changedFileMap, fileActions =
                                computeReloadActions symbols changedFileMap topFileToReload

                            let tmp = Path.GetTempPath()

                            for fileName, fileAction in fileActions do
                                match fileAction with
                                | ReloadCompletely -> evalScript fileName
                                | ReloadChangedOnly ->
                                    let fileData = changedFileDataMap[fileName]
                                    let oldSymbols = symbols[fileName].Symbols
                                    let newSymbols = changedFileMap[fileName].Symbols

                                    let text =
                                        CodeGeneration.generateCode
                                            fileData.Source
                                            fileData.ParseResults
                                            oldSymbols
                                            newSymbols

                                    let fileName = Path.Join(tmp, Path.GetFileName fileName)
                                    File.WriteAllText(fileName, text)
                                    evalScript fileName
                                //File.Delete fn
                                | NoAction -> ()

                            reply.Reply(Ok())

                            return! messageLoop sln changedFileMap Set.empty
                        with ex ->
                            reply.Reply(Error ex)
                            return! messageLoop sln symbols changedFiles
                    | _ -> return! messageLoop sln symbols changedFiles
                }

            and init () =
                async {
                    let! fileMap = getSymbolsForProjects fsChecker sln.FsProjects
                    return! messageLoop sln fileMap Set.empty
                }

            init ())

    let makeWatcher (agent: MailboxProcessor<Message>) sln =
        let path = ProjectLoading.getTopPath sln
        let watcher = new FileSystemWatcher(path)
        watcher.IncludeSubdirectories <- true
        watcher.EnableRaisingEvents <- true
        watcher.NotifyFilter <- NotifyFilters.LastWrite ||| NotifyFilters.FileName

        watcher.Changed.Add(fun e -> agent.Post(FileChanged e.FullPath))

        watcher // keep a reference to avoid GC


#warn "57"
