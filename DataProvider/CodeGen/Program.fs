// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open SturmovikMission.DataProvider.CodeGenerator

type Settings = {
    Input : string
    Output : string
    GeneratedNamespace : string
    PrintUsage : bool
}
with
    static member Default =
        {
            Input = ""
            Output = ""
            GeneratedNamespace = "SturmovikMission"
            PrintUsage = false
        }

    static member FromArgv (argv : string[]) =
        let rec work argv settings =
            match argv with
            | "-i" :: path :: rest ->
                { settings with
                    Input = path
                }
                |> work rest
            | "-o" :: path :: rest ->
                { settings with
                    Output = path
                }
                |> work rest
            | "-n" :: ns :: rest ->
                { settings with
                    GeneratedNamespace = ns
                }
                |> work rest
            | ("-h" | "--help" | "/?") :: rest ->
                { Settings.Default with PrintUsage = true }
            | [] ->
                settings
            | invalidArg :: _ ->
                eprintfn "Invalid argument '%s' or missing parameter" invalidArg
                { Settings.Default with PrintUsage = true }
        work (List.ofArray argv) Settings.Default

[<EntryPoint>]
let main argv =
    let settings = Settings.FromArgv argv
    if settings.PrintUsage then
        """
CodeGen -i <sample mission file> -o <output source code file> -n <namespace>

Analyze IL-2 Sturmovik: Great Battles sample mission file to infer schema,
and generate data types.
        """
        |> printfn "%s"
        2
    else
        if String.IsNullOrWhiteSpace(settings.Input) then
            eprintfn "Missing input file (-i)"
            1
        else
        if not (IO.File.Exists(settings.Input)) then
            eprintfn "Cannot open '%s' for reading" settings.Input
            1
        else
        let sourceCode = generateCode(settings.GeneratedNamespace, settings.Input)
        if String.IsNullOrWhiteSpace(settings.Output) then
            printfn "%s" sourceCode
            0
        else
        try
            IO.File.WriteAllText(settings.Output, sourceCode)
            0
        with exc ->
            eprintfn "Uncaught exception: '%s'" exc.Message
            1
