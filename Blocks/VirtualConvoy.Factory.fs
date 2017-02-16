﻿/// Put together instances and predicates of virtual convoys.
module SturmovikMission.Blocks.VirtualConvoy.Factory

open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.Links
open SturmovikMission.Blocks.Predicates
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.BlocksMissionData

// Types for each instance type.
// Those are typically typed ints, but could be typed strings, or any other type suitable for a dictionary key.
type ConvoyInstance = ConvoyInstance of int
type WhileEnemyCloseInstance = WhileEnemyCloseInstance of int
type ActiveWaypointInstance = ActiveWaypointInstance of int
type TruckInConvoyInstance = TruckInConvoyInstance of convoy: int * pos: int
type TimerInstance = TimerInstance of int

/// <summary>
/// Type used in the arguments of VirtualConvoy.Create. Denotes one vertex of the path of the virtual convoy.
/// </summary>
type PathVertex =
    { Pos : Mcu.Vec3
      Ori : Mcu.Vec3
      Radius : int
      Speed : int
      Priority : int
    }

/// <summary>
/// A virtual convoy, i.e. a convoy that does not actually exist until an enemy approaches its
/// expected position. The intent is to provide the illusion of a mission filled with large numbers
/// of planes and vehicles, while keeping CPU utilization of the server to a low level.
/// See Proto.txt.
/// </summary>
type VirtualConvoy =
    { ConvoySet : Map<ConvoyInstance, Convoy>
      TruckInConvoy : Set<ConvoyInstance * int * TruckInConvoyInstance>
      WhileEnemyCloseOfConvoy : Set<ConvoyInstance * WhileEnemyCloseInstance>
      
      TruckInConvoySet : Map<TruckInConvoyInstance, TruckInConvoy>
      
      ActiveWaypointSet : Map<ActiveWaypointInstance, ActiveWaypoint>
      ConvoyAtWaypoint : Set<ActiveWaypointInstance * ConvoyInstance>
      EnemyCloseAtWaypoint : Set<ActiveWaypointInstance * WhileEnemyCloseInstance>
      Path : Set<ActiveWaypointInstance * ActiveWaypointInstance>
      PathStart : Set<ActiveWaypointInstance>
      PathEnd : Set<ActiveWaypointInstance>
      TimerBetweenWaypoints : Set<ActiveWaypointInstance * ActiveWaypointInstance * TimerInstance>

      WhileEnemyCloseSet : Map<WhileEnemyCloseInstance, WhileEnemyClose>
      ConvoyOfEnemyClose : Set<WhileEnemyCloseInstance * ConvoyInstance>

      TimerSet : Map<TimerInstance, Timer>

      Api : ConvoyControl
    }
with
    interface McuUtil.IMcuGroup with
        member this.Content = []
        member this.LcStrings = []
        member this.SubGroups =
            [
                for kvp in this.ConvoySet do
                    yield kvp.Value.All
                for kvp in this.TruckInConvoySet do
                    yield kvp.Value.All
                for kvp in this.ActiveWaypointSet do
                    yield kvp.Value.All
                for kvp in this.WhileEnemyCloseSet do
                    yield kvp.Value.All
                for kvp in this.TimerSet do
                    yield kvp.Value.All
                yield this.Api.All
            ]

    /// <summary>
    /// Create the instances and relations of a virtual convoy.
    /// </summary>
    /// <param name="store">Numerical ID store. All MCUs in a mission must be created using the same store to avoid duplicate identifiers.</param>
    /// <param name="path">Path followed by the convoy.</param>
    /// <param name="convoySize">Number of vehicle/planes in the column or wing.</param>
    static member Create(store : NumericalIdentifiers.IdStore, path : PathVertex list, convoySize : int, country : Mcu.CountryValue, coalition : Mcu.CoalitionValue) =
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = T.GroupData(Parsing.Stream.FromFile "Blocks.Mission").CreateMcuList()
        let palette = McuUtil.filterByPath ["Palette"] db |> List.ofSeq
        for mcu in palette do
            subst mcu

        let missionStart = getTriggerByName palette T.Blocks.``Translator Mission Begin``
        let convoySet =
            seq {
                for i, vertex in Seq.zip (Seq.initInfinite id) path do
                    yield (ConvoyInstance i, Convoy.Create(store, vertex.Pos, vertex.Ori, country))
            }
            |> Map.ofSeq
        let truckInConvoySet =
            seq {
                for i, vertex in Seq.zip (Seq.initInfinite id) path do
                    for pos in 1..convoySize do
                        yield (TruckInConvoyInstance(i, pos), TruckInConvoy.Create(store, vertex.Pos, vertex.Ori, pos, country))
            }
            |> Map
        let whileEnemyCloseSet =
            seq {
                for i, vertex in Seq.zip (Seq.initInfinite id) path do
                    yield (WhileEnemyCloseInstance i, WhileEnemyClose.Create(store, vertex.Pos, coalition))
            }
            |> Map.ofSeq
        let activeWaypointSet =
            seq {
                for i, vertex in Seq.zip (Seq.initInfinite id) path do
                    yield (ActiveWaypointInstance i, ActiveWaypoint.Create(store, vertex.Pos, vertex.Ori, vertex.Speed, vertex.Priority))
            }
            |> Map.ofSeq
        let timerSet =
            seq {
                for i, (curr, next) in Seq.zip (Seq.initInfinite id) (Seq.pairwise path) do
                    let expectedTime =
                        let vec =
                            McuUtil.vecMinus next.Pos curr.Pos
                        let distance = sqrt(vec.X * vec.X + vec.Y * vec.Y + vec.Z * vec.Z)
                        3.6 * distance / float next.Speed
                    yield (TimerInstance i, Timer.Create(store, curr.Pos, 1.1 * expectedTime))
            }
            |> Map.ofSeq

        let truckInConvoy =
            seq {
                for i, vertex in Seq.zip (Seq.initInfinite id) path do
                    for pos in 1..convoySize do
                        yield (ConvoyInstance i, pos, TruckInConvoyInstance(i, pos))
            }
            |> Set.ofSeq
        let whileEnemyCloseOfConvoy =
            path
            |> List.mapi(fun i _ -> (ConvoyInstance i, WhileEnemyCloseInstance i))
            |> Set.ofList
        let convoyAtWaypoint =
            path
            |> List.mapi(fun i _ -> (ActiveWaypointInstance i, ConvoyInstance i))
            |> Set.ofList
        let enemyCloseAtWaypoint =
            path
            |> List.mapi(fun i _ -> (ActiveWaypointInstance i, WhileEnemyCloseInstance i))
            |> Set.ofList
        let path2 =
            path
            |> Seq.mapi(fun i _ -> ActiveWaypointInstance i)
            |> Seq.pairwise
            |> Set.ofSeq
        let pathStart = Set [ ActiveWaypointInstance 0 ]
        let pathEnd = Set [ ActiveWaypointInstance (List.length path - 1) ]
        let timerBetweenWaypoints =
            path2
            |> Seq.map (fun (ActiveWaypointInstance i as curr, next) -> (curr, next, TimerInstance i))
            |> Set.ofSeq
        let convoyOfEnemyClose =
            activeWaypointSet
            |> Map.map (fun wp _ -> (get enemyCloseAtWaypoint wp, get convoyAtWaypoint wp))
            |> Map.toSeq
            |> Seq.map snd
            |> Set.ofSeq
        let apiPos =
            path
            |> List.fold (fun sum vertex -> McuUtil.translate sum vertex.Pos) (McuUtil.newVec3(0.0, 0.0, 0.0))
        let apiPos =
            let n = List.length path |> float
            McuUtil.newVec3(apiPos.X / n, apiPos.Y / n, apiPos.Z / n)
        let api = ConvoyControl.Create(store, apiPos, convoySize)
        { ConvoySet = convoySet
          TruckInConvoy = truckInConvoy
          WhileEnemyCloseOfConvoy = whileEnemyCloseOfConvoy
          TruckInConvoySet = truckInConvoySet
          ActiveWaypointSet = activeWaypointSet
          ConvoyAtWaypoint = convoyAtWaypoint
          EnemyCloseAtWaypoint = enemyCloseAtWaypoint
          Path = path2
          PathStart = pathStart
          PathEnd = pathEnd
          TimerBetweenWaypoints = timerBetweenWaypoints
          WhileEnemyCloseSet = whileEnemyCloseSet
          ConvoyOfEnemyClose = convoyOfEnemyClose
          TimerSet = timerSet
          Api = api
        }

    /// <summary>
    /// Create the links between the MCUs in the virtual convoy.
    /// </summary>
    member this.CreateLinks() =
        let columns =
            [
                for convoy, position, truck in this.TruckInConvoy do
                    yield this.TruckInConvoySet.[truck].Entity, this.ConvoySet.[convoy].LeadCarEntity, position
            ]
        let objectLinks =
            [
                for wp, convoy in this.ConvoyAtWaypoint do
                    for wp2 in star this.Path wp do
                        yield this.ActiveWaypointSet.[wp2].Waypoint :> Mcu.McuTrigger, this.ConvoySet.[convoy].LeadCarEntity :> Mcu.McuBase 
                for wec, convoy in this.ConvoyOfEnemyClose do
                    yield this.WhileEnemyCloseSet.[wec].Proximity, this.ConvoySet.[convoy].LeadCarEntity :> Mcu.McuBase
            ]
        let targetLinks =
            [
                for wp, convoy in this.ConvoyAtWaypoint do
                    for wp2 in star this.Path wp do
                        yield this.ConvoySet.[convoy].TriggerGates, this.ActiveWaypointSet.[wp].Gate :> Mcu.McuBase
                for wec, convoy in this.ConvoyOfEnemyClose do
                    yield this.WhileEnemyCloseSet.[wec].WakeUp, this.ConvoySet.[convoy].ActivateGroup :> Mcu.McuBase
                    yield this.WhileEnemyCloseSet.[wec].Sleep, this.ConvoySet.[convoy].DeactivateGroup :> Mcu.McuBase
                for curr, next in this.Path do
                    let convoy = get this.ConvoyAtWaypoint curr
                    let convoy2 = get this.ConvoyAtWaypoint next
                    yield this.ConvoySet.[convoy].LeadCarDamaged, this.ConvoySet.[convoy2].DeleteLeadCar :> Mcu.McuBase
                    yield this.ConvoySet.[convoy].DeleteLeadCar, this.ConvoySet.[convoy2].DeleteLeadCar :> Mcu.McuBase
                    for _, pos, truck in filter3 this.TruckInConvoy (Some convoy, None, None) do
                        for _, _, truck2 in filter3 this.TruckInConvoy (Some convoy2, Some pos, None) do
                            yield this.TruckInConvoySet.[truck].Damaged, this.TruckInConvoySet.[truck2].Delete :> Mcu.McuBase
                            yield this.TruckInConvoySet.[truck].Delete, this.TruckInConvoySet.[truck2].Delete :> Mcu.McuBase
                for wp, convoy in this.ConvoyAtWaypoint do
                    let wec = get this.WhileEnemyCloseOfConvoy convoy
                    let wec = this.WhileEnemyCloseSet.[wec]
                    let wp = this.ActiveWaypointSet.[wp]
                    yield wp.Activate, wec.StartMonitoring :> Mcu.McuBase
                    yield wp.Deactivate, wec.StopMonitoring :> Mcu.McuBase
                for curr, next in this.Path do
                    let curr2 = this.ActiveWaypointSet.[curr]
                    let next2 = this.ActiveWaypointSet.[next]
                    yield curr2.Waypoint :> Mcu.McuTrigger, next2.Waypoint :> Mcu.McuBase
                    for _, _, timer in filter3 this.TimerBetweenWaypoints (Some curr, Some next, None) do
                        let timer = this.TimerSet.[timer]
                        yield timer.Elapsed, next2.Activate :> Mcu.McuBase
                        yield timer.Elapsed, curr2.Deactivate :> Mcu.McuBase
                        yield curr2.Activate, timer.Start :> Mcu.McuBase
                for curr, next in this.Path do
                    let currWec = get this.EnemyCloseAtWaypoint curr
                    let currWec = this.WhileEnemyCloseSet.[currWec]
                    for wp in star this.Path next do
                        let afterWec = get this.EnemyCloseAtWaypoint wp
                        let afterWec = this.WhileEnemyCloseSet.[afterWec]
                        yield currWec.WakeUp, afterWec.Deactivate :> Mcu.McuBase
                        yield currWec.Sleep, afterWec.Activate :> Mcu.McuBase
                for wp in this.PathStart do
                    yield this.Api.Start, this.ActiveWaypointSet.[wp].Activate :> Mcu.McuBase
                for convoy, _, truck in this.TruckInConvoy do
                    yield this.TruckInConvoySet.[truck].Damaged, this.Api.Destroyed :> Mcu.McuBase
                for wp in this.PathEnd do
                    let finish = this.ActiveWaypointSet.[wp]
                    yield finish.Waypoint :> Mcu.McuTrigger, this.Api.Arrived :> Mcu.McuBase
                    yield finish.Activate, this.Api.Arrived :> Mcu.McuBase

            ]
        { Columns = columns
          Objects = objectLinks
          Targets = targetLinks
        }