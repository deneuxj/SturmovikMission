module Logging

open System
open System.IO

let private getLogDir() =
    let localAppData = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData)
    let logDir = Path.Combine(localAppData, "SturmovikMission.DataProvider")
    logDir

let initLogging() =
    let logDir = getLogDir()
    if not <| Directory.Exists(logDir) then
        try
            Directory.CreateDirectory(logDir)
            |> ignore
            |> Ok
        with
        | e -> 
            sprintf "Failed to create directory '%s' where log files are created: '%s'" logDir e.Message
            |> Error
    else
        Ok()

let openLogFile() =
    let now = DateTime.UtcNow
    let filename = sprintf "log-%s-%s" (now.ToShortDateString()) (now.ToShortTimeString())
    let path = Path.Combine(getLogDir(), filename)
    try
        File.OpenWrite(path)
        |> Ok
    with
    | e ->
        sprintf "Failed to open or create log file '%s': '%s'" path e.Message
        |> Error
    |> Result.map (fun s ->
        s.Seek(0L, SeekOrigin.End) |> ignore
        new StreamWriter(s)
    )

let log (out : Result<StreamWriter, _>) (msg : string) =
    match out with
    | Error _ -> ()
    | Ok out ->
        let now = DateTime.UtcNow
        out.WriteLine(sprintf "%s: %s" (now.ToShortTimeString()) msg)
        out.Flush()
        out.BaseStream.Flush()

let closeLog (out : Result<StreamWriter, _>) =
    match out with
    | Error _ -> ()
    | Ok out ->
        out.Close()