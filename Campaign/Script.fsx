﻿// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r "../DataProvider/bin/Debug/DataProvider.dll"
#r "../Blocks/bin/Debug/SturmovikMission.Blocks.exe"
#r "../Campaign/bin/Debug/Campaign.dll"
#r "System.Numerics.Vectors"

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.MissionGeneration
open Campaign.AutoOrder
open System.IO

try
    File.Delete("StrategySmall1.mission")
    File.Copy(@"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\StrategySmall1.mission", "StrategySmall1.mission")
    File.Delete("Blocks.mission")
    File.Copy(@"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\Blocks.mission", "Blocks.mission")
    File.Delete("Vehicles.mission")
    File.Copy(@"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\Vehicles.mission", "Vehicles.mission")
with
    | exc -> printfn "Error copying files: '%s'" exc.Message

let random = System.Random(0)
let strategyFile = "StrategySmall1.mission"
let world0, blocks, bridges, options = World.Create(strategyFile)
let world = { world0 with WeatherDaysOffset = 15.0 * (random.NextDouble() - 0.5)}
let state = WorldState.Create(world, strategyFile)
let axisConvoyOrders =
    createAllConvoyOrders(Axis, world, state)
    |> prioritizeConvoys 4 world state
let alliesConvoyOrders =
    createAllConvoyOrders(Allies, world, state)
    |> prioritizeConvoys 4 world state
let allConvoyOrders = axisConvoyOrders @ alliesConvoyOrders
let axisInvasionOrderCandidates =
    createGroundInvasionOrders(Some Axis, world, state)
    |> prioritizeGroundInvasionOrders(world, state)
    |> List.truncate 3
let alliesInvasionOrderCandidates =
    createGroundInvasionOrders(Some Allies, world, state)
    |> prioritizeGroundInvasionOrders(world, state)
    |> List.truncate 3
let axisReinforcementOrderCandidates =
    createReinforcementOrders(Some Axis, world, state)
let alliesReinforcementOrderCandidates =
    createReinforcementOrders(Some Allies, world, state)
let axisNeededReinforcementOrders, safeAxisInvasionOrders = filterIncompatible(axisReinforcementOrderCandidates, axisInvasionOrderCandidates)
let alliesNeededReinforcementOrders, safeAlliesInvasionOrders = filterIncompatible(alliesReinforcementOrderCandidates, alliesInvasionOrderCandidates)
let axisInvasionOrders =
    safeAxisInvasionOrders
    |> List.truncate 1
    |> List.map (fun (order, _, _) -> order)
let alliesInvasionOrders =
    safeAlliesInvasionOrders
    |> List.truncate 1
    |> List.map (fun (order, _, _) -> order)
let axisReinforcementOrders = axisNeededReinforcementOrders |> List.truncate 1
let alliesReinforcementOrders = alliesNeededReinforcementOrders |> List.truncate 1
let allInvasionOrders =
    axisInvasionOrders @ alliesInvasionOrders
let allReinforcementOrders =
    axisReinforcementOrders @ alliesReinforcementOrders
let missionName = "AutoGenMission2"
let author = "coconut"
let briefing = "Work in progress<br><br>Test of dynamically generated missions<br><br>"
let outputDir = @"C:\Users\johann\Documents\AutoMoscow"
writeMissionFile random author missionName briefing 120 60 options blocks bridges world state allConvoyOrders allInvasionOrders allReinforcementOrders (Path.Combine(outputDir, missionName + ".Mission"))

let serverDataDir = @"E:\dserver\data"
let serverBinDir = @"E:\dserver\bin"
let mpDir = Path.Combine(serverDataDir, "Multiplayer")
let swallow f = try f() with | _ -> ()
swallow (fun () -> File.Delete (Path.Combine(mpDir, missionName + ".Mission")))
swallow (fun () -> File.Delete (Path.Combine(mpDir, missionName + ".eng")))
swallow (fun () -> File.Delete (Path.Combine(mpDir, missionName + ".msnbin")))
File.Copy(Path.Combine(outputDir, missionName + ".Mission"), Path.Combine(mpDir, missionName + ".Mission"))
File.Copy(Path.Combine(outputDir, missionName + ".eng"), Path.Combine(mpDir, missionName + ".eng"))
open System.Diagnostics
let p = ProcessStartInfo("MissionResaver.exe", sprintf "-d %s -f %s" serverDataDir (Path.Combine(mpDir, missionName + ".Mission")))
p.WorkingDirectory <- Path.Combine(serverBinDir, "resaver")
p.UseShellExecute <- true
let proc = Process.Start(p)
proc.WaitForExit()
swallow (fun () -> File.Delete (Path.Combine(mpDir, missionName + ".Mission")))
