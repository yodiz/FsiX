module FsiX.ProjectReloading.SymbolParsing

open FSharp.Compiler.Syntax
open FSharpPlus
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharpPlus.Control

open System

#nowarn "57"


[<Struct>]
type NamespacePath =
    | NamespacePath of string list

    static member ofString str =
        String.split [ "." ] str |> Seq.rev |> Seq.toList |> NamespacePath

    static member add x (NamespacePath p) = NamespacePath(x :: p)

    static let longIdToList (l: LongIdent) =
        l |> Seq.rev |> Seq.map _.idText |> Seq.toList
    static let longIdToListFantomas (l: Fantomas.FCS.Syntax.LongIdent) =
        l |> Seq.rev |> Seq.map _.idText |> Seq.toList

    static member addLongId l (NamespacePath p) =
        NamespacePath(List.append (longIdToList l) p)

    static member ofLongId l = NamespacePath <| longIdToList l
    static member ofLongIdFantomas l = NamespacePath <| longIdToListFantomas l

    member this.IsIn(NamespacePath biggerPath) =
        match this with
        | NamespacePath subPath -> subPath = (List.zipShortest subPath biggerPath |> List.map snd)

    override this.ToString() =
        match this with
        | NamespacePath p -> p |> List.rev |> String.concat "."

module SymbolExtraction =

    type SymbolType =
        | TypeDef
        | LetDef
        | ExnDef

    type ParseResult =
        { Range: range
          Path: NamespacePath
          SymbolType: SymbolType }

    let getLets path range ((letDefinitions: SynBinding list)) =
        let getPath =
            function
            | SynPat.Named(SynIdent(ident = i), _, _, _) -> NamespacePath.add i.idText path
            | SynPat.LongIdent(SynLongIdent(id = i), _, _, _, _, _) -> NamespacePath.addLongId i path
            | _ -> failwith "todo"

        let toParseResult synPat =
            { Range = range
              SymbolType = LetDef
              Path = getPath synPat }

        List.map (fun (SynBinding(headPat = p)) -> toParseResult p) letDefinitions

    let getTypes path range (typeDefinitions: SynTypeDefn list) =
        let getPath =
            function
            | SynComponentInfo(longId = l) -> NamespacePath.addLongId l path

        let toParseResult synPat =
            { Range = range
              SymbolType = TypeDef
              Path = getPath synPat }

        List.map (fun (SynTypeDefn(typeInfo = t)) -> toParseResult t) typeDefinitions

    let getException path range (SynExceptionDefn(exnRepr = exnRepr)) =
        let getName =
            function
            | SynExceptionDefnRepr(caseName = SynUnionCase(ident = (SynIdent(ident = i)))) -> i.idText

        { Range = range
          SymbolType = ExnDef
          Path = NamespacePath.add (getName exnRepr) path }

    let rec getSymbols' path =
        function
        | SynModuleDecl.Types(range = range; typeDefns = typeDefs) ->
            let x = range.GetHashCode
            getTypes path range typeDefs
        | SynModuleDecl.Let(range = range; bindings = letDefs) -> getLets path range letDefs
        | SynModuleDecl.Exception(range = range; exnDefn = exnDefn) -> [ getException path range exnDefn ]
        | SynModuleDecl.NestedModule(decls = decls; moduleInfo = info) ->
            let id =
                match info with
                | SynComponentInfo(longId = longId) -> NamespacePath.addLongId longId path

            List.collect (getSymbols' id) decls
        | _ -> []

    let getSymbols moduleIdent codeLines =
        List.collect (getSymbols' (NamespacePath.ofLongId moduleIdent)) codeLines



module SymbolHashing =
    type SymbolWithHash =
        { Symbol: SymbolExtraction.ParseResult
          Hash: int }

    let rangeToInterval (text: string) (range: range) =
        let getLineIndex charIndex linesAmount =
            let rec getLineIndex' i lineCounter =
                match text[i] with
                | '\n' when lineCounter >= (linesAmount - 1) -> i
                | '\n' -> getLineIndex' (i + 1) (lineCounter + 1)
                | _ -> getLineIndex' (i + 1) lineCounter

            getLineIndex' charIndex 1

        let startLineI = getLineIndex 0 range.StartLine
        let startI = startLineI + range.StartColumn

        if range.StartLine = range.EndLine then
            startI, startLineI + range.EndColumn + 1
        else
            let endLineI = getLineIndex (startLineI + 1) (range.EndLine - range.StartLine + 1)
            let endI = endLineI + range.EndColumn + 1
            (startI, endI)

    let intervalToHash (text: string) (startIndex, endIndex) =
        let hash = HashCode()

        for i = startIndex to endIndex do
            hash.Add(text[i])

        hash.ToHashCode()

    let rangeToHash text range =
        rangeToInterval text range |> intervalToHash text

    let rangeToHash' (text: ISourceText) range =
        text.GetSubTextFromRange range |> _.GetHashCode()

    let getRangeHashesForFile (source: ISourceText) (symbolDefinitions: SymbolExtraction.ParseResult seq) =
        symbolDefinitions
        |> Seq.map (fun def ->
            { Symbol = def
              Hash = rangeToHash' source def.Range })

type NamespaceString = String
type FileNameString = String

type FileParseResults =
    { FileName: string
      Symbols: Map<NamespaceString, SymbolHashing.SymbolWithHash>
      DependencyFiles: string list }

type ProjectParseResults = Map<FileNameString, FileParseResults>



let fileFilter (fileName: FileNameString) =
    not (fileName.EndsWith "AssemblyInfo.fs" || fileName.EndsWith "AssemblyAttributes.fs")
    && fileName.EndsWith ".fs"

module FileBasedExtraction =
    type FileParseData =
        { FileName: string
          ParseResults: FSharpParseFileResults
          Source: ISourceText
          CheckResults: FSharpCheckFileAnswer
          Symbols: Map<NamespaceString, SymbolHashing.SymbolWithHash>
          SymbolUses: FSharp.Compiler.Symbols.FSharpSymbol seq }

    let getFileData
        (snapshot: FSharpProjectSnapshot)
        (checker: FSharpChecker)
        (fileName: string)
        : Async<FileParseData> =

        async {
            let! parseRes, checkRes = checker.ParseAndCheckFileInProject(fileName, snapshot)

            let uses =
                match checkRes with
                | FSharpCheckFileAnswer.Succeeded r ->
                    r.GetAllUsesOfAllSymbolsInFile() |> Seq.map _.Symbol |> Seq.distinct
                | _ -> Seq.empty

            let (ParsedInput.ImplFile(ParsedImplFileInput(contents = contents))) =
                parseRes.ParseTree

            let [ SynModuleOrNamespace(decls = codeLines; longId = l) ] = contents
            let defs = SymbolExtraction.getSymbols l codeLines

            let! source =
                snapshot.SourceFiles
                |> Seq.find (fun t -> t.FileName = fileName)
                |> _.GetSource()
                |> Async.AwaitTask

            let symbolsWithHashes =
                SymbolHashing.getRangeHashesForFile source defs
                |> Seq.map (fun x -> x.Symbol.Path.ToString(), x)
                |> Map.ofSeq


            return
                { FileName = fileName
                  ParseResults = parseRes
                  CheckResults = checkRes
                  Symbols = symbolsWithHashes
                  SymbolUses = uses
                  Source = source }
        }

    let getFileDataMap snapshot checker fileNames =
        fileNames |> Seq.map (getFileData snapshot checker) |> Async.Parallel
        |>> Seq.map (fun f -> f.FileName, f)
        |>> Map.ofSeq

    let getSymbolsForFiles (fileDataMap: Map<FileName, FileParseData>) =

        let getSymbolFile (fcsSymbol: FSharp.Compiler.Symbols.FSharpSymbol) =
            monad {
                let! impl = fcsSymbol.ImplementationLocation
                let! file = Map.tryFind impl.FileName fileDataMap

                match Map.tryFind fcsSymbol.FullName file.Symbols with
                | None -> return! None
                | Some _ -> return impl.FileName
            }

        fileDataMap
        |> Map.map (fun fileName file ->
            { FileName = fileName
              Symbols = file.Symbols
              DependencyFiles =
                file.SymbolUses
                |> Seq.choose getSymbolFile
                |> Seq.distinct
                |> Seq.filter ((<>) fileName)
                |> Seq.toList })


let getSymbolsForProjects (checker: FSharpChecker) (fsOpts: FSharpProjectOptions seq) =
    async {
        let mapForSingleProject (fsOpts: FSharpProjectOptions) =
            async {
                let! snapshot = FSharpProjectSnapshot.FromOptions(fsOpts, DocumentSource.FileSystem)
                let! checkResults = checker.ParseAndCheckProject snapshot
                let files = checkResults.DependencyFiles |> Seq.filter fileFilter |> Set.ofSeq
                return! FileBasedExtraction.getFileDataMap snapshot checker files
            }

        let! fileDataMap =
            fsOpts |> Seq.map mapForSingleProject |> Async.Parallel
            |>> Array.reduce Map.union

        return FileBasedExtraction.getSymbolsForFiles fileDataMap
    }

#warn "57"
