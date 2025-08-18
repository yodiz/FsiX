module FsiX.Features.Reloading
open System
open System.Reflection

open FsiX.ProjectLoading


//todo 
//load all methods, put them in hash by name
//also include their path when it's in modules
//and return type & args
//when user enters method with same name, type, try to reload it
//if it finds multiple, check one for last opened file
//otherwise give warming
//if none found with same type, give hint it's not possible to overload types
//otherwise print nothing

type Method = {
  MethodInfo: MethodInfo
  FullName: string
} with static member make modulePath (m : MethodInfo) = 
        { MethodInfo = m;
          FullName = m.Name :: modulePath |> Seq.rev |> String.concat "." }
type State = {
  Methods: Map<string, Method list>
  LastOpenModules: string list
}

type Event = 
  | NewReplAssemblies of Assembly array
  | ModuleOpened of string

let getAllMethods (asm: Assembly) =
  let rec getMethods currentPath (t: Type) =
    let newPath =
      if t.Name.Contains "FSI_" then
        currentPath
      else 
        t.Name :: currentPath
    let methods = 
        t.GetMethods()
        |> Seq.filter (fun m -> m.IsStatic && not <| m.IsGenericMethod)
        |> Seq.map (Method.make newPath)
    let types = t.GetNestedTypes() |> Seq.toList
    methods 
    |> Seq.append (Seq.collect (getMethods (t.Name :: currentPath)) types)
  asm.GetExportedTypes() 
  |> Seq.collect (getMethods [])

let mkReloadingState (sln: FsiX.ProjectLoading.Solution) = 
  let assemblies = 
    sln.Projects |> Seq.map (_.TargetPath >> Assembly.LoadFrom)
  let methods = 
    assemblies
    |> Seq.collect getAllMethods
    |> Seq.groupBy _.MethodInfo.Name
    |> Seq.map (fun (methodName, methods) -> methodName, List.ofSeq methods)
    |> Map.ofSeq
  {Methods = methods; LastOpenModules = []}

open FSharpPlus

open HarmonyLib
let detourMethod (method: MethodBase) (replacement: MethodBase) = 
  typeof<Harmony>.Assembly 
  |> _.GetTypes()
  |> Seq.find (fun t -> t.Name = "PatchTools")
  |> fun x -> x.GetDeclaredMethods() 
  |> Seq.find (fun n -> n.Name = "DetourMethod") 
  |> fun x -> x.Invoke(null, [|method; replacement|])
  |> ignore

let handleNewAsmFromRepl (asm: Assembly) (st: State) = 
  for m in getAllMethods asm do
    let potentialReplacement = 
      Map.tryFind m.MethodInfo.Name st.Methods
      >>= (
        Seq.filter (fun existingMethod ->
          let getParams m = m.MethodInfo.GetParameters() |> Array.map _.ParameterType
          getParams existingMethod = getParams m
          && existingMethod.MethodInfo.ReturnType = m.MethodInfo.ReturnType
          && existingMethod.FullName.Contains m.FullName
        ) 
        >> Seq.sortByDescending (fun existingMethod -> 
            let lastOpenedModuleName = 
              match st.LastOpenModules with
              | x :: _ -> x
              | _ -> ""
            FuzzySharp.Fuzz.Ratio(lastOpenedModuleName + m.FullName, existingMethod.FullName)
        )
        >> Seq.tryHead
      )
    match potentialReplacement with
    | None -> ()
    | Some methodToReplace ->
      Console.WriteLine("\u001b[90m Updating method " + methodToReplace.FullName + "\u001b[0m")
      detourMethod methodToReplace.MethodInfo m.MethodInfo
      ()
