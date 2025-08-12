module FsiX.ProjectLoading

open System.IO

open FSharp.Compiler.CodeAnalysis
open Ionide.ProjInfo

open Ionide.ProjInfo.Types


type FileName = string

type Solution =
    { FsProjects: FSharpProjectOptions list
      Projects: ProjectOptions list
      Files: Map<FileName, FSharpProjectOptions> }

let loadSolution (projectDirectory: string) =
    let projectFile =
        projectDirectory
        |> Directory.EnumerateFiles
        |> Seq.find (fun s -> s.EndsWith ".fsproj" || s.EndsWith ".sln")

    let projectDirectory: DirectoryInfo = Directory.GetParent projectFile
    let toolsPath = Init.init projectDirectory None
    let defaultLoader: IWorkspaceLoader = WorkspaceLoader.Create(toolsPath, [])

    let projects =
        match projectFile with
        | filePath when filePath.EndsWith ".sln" -> defaultLoader.LoadSln filePath
        | filePath when filePath.EndsWith ".fsproj" -> defaultLoader.LoadProjects [ filePath ]
        | _ -> failwith "Provide sln or fsproj"

    let fcsProjectOptions = List.ofSeq <| FCS.mapManyOptions projects

    let fileMap =
        fcsProjectOptions
        |> Seq.collect (fun prj -> prj.SourceFiles |> Seq.map (fun file -> (file, prj)))
        |> Map.ofSeq

    { FsProjects = fcsProjectOptions
      Projects = projects |> Seq.toList
      Files = fileMap }

let getSourceOrderedFiles sln =
    sln.FsProjects
    |> Seq.rev
    |> Seq.collect (fun proj -> proj.SourceFiles |> Seq.map (fun file -> file, proj))
    |> List.ofSeq

let solutionToFsiArgs (sln: Solution) =
    let dlls = sln.Projects |> Seq.map _.TargetPath |> Seq.rev |> Seq.toList

    let nugets =
        sln.Projects
        |> Seq.collect _.PackageReferences
        |> Seq.map _.FullPath
        |> Seq.distinct
        |> Seq.toList

    [| "fsi"; yield! nugets |> Seq.append dlls |> Seq.map (sprintf "-r:%s") |]

let getTopPath' (paths: string list) =
    let splitPath (p: string) =
        Directory.GetParent p
        |> _.FullName
        |> _.TrimEnd(Path.DirectorySeparatorChar)
        |> _.Split(Path.DirectorySeparatorChar)

    let allParts = paths |> List.map splitPath
    let minLength = allParts |> List.minBy Array.length |> Array.length

    let commonParts =
        [ 0 .. minLength - 1 ]
        |> Seq.takeWhile (fun i -> allParts |> List.forall (fun parts -> parts[i] = allParts.Head[i]))
        |> Seq.map (fun i -> allParts.Head.[i])
        |> Seq.toArray

    Path.Combine commonParts |> fun x -> $"/{x}"

let getTopPath (sln: Solution) =
    sln.Projects |> List.map _.ProjectFileName |> getTopPath'
