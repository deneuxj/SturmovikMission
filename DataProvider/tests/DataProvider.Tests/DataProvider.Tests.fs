//    Copyright 2019 Johann Deneux
//
//    This file is part of SturmovikMission.
//
//    SturmovikMission is free software: you can redistribute it and/or modify
//    it under the terms of the GNU Lesser General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    SturmovikMission is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU Lesser General Public License for more details.
//
//    You should have received a copy of the GNU Lesser General Public License
//    along with SturmovikMission.  If not, see <http://www.gnu.org/licenses/>.
//
//

module SturmovikMission.DataProvider.Test

open SturmovikMissionTypes
open NUnit.Framework

type T = Provider< @"..\..\..\data\Sample.Mission">

[<Test>]
let ``T.Vehicle has repair and refuel fields``() =
    let vehicle =
        T.Vehicle.Default
            .SetMaintenanceRadius(T.Integer.N 500)
            .SetRepairFriendlies(T.Boolean.N true)
            .SetRepairTime(T.Integer.N 60)
            .SetRefuelFriendlies(T.Boolean.N true)
            .SetRefuelTime(T.Integer.N 30)
    let repr = vehicle.AsString()
    let parser = T.Vehicle.GetParser()
    parser.Run(Parsing.Stream.FromString repr) |> ignore
    Assert.DoesNotThrow(fun () ->
        parser.Run(Parsing.Stream.FromString repr)
        |> ignore)

[<Test>]
let ``T.Airfield.Planes can be constructed``() =
    Assert.DoesNotThrow(fun () ->
        T.Airfield.Planes.Default
        |> ignore)

[<Test>]
let ``T.Options.WindLayers.WindLayers_ValueType can be default-constructed``() =
    Assert.DoesNotThrow(fun () ->
        T.Options.WindLayers.WindLayers_ValueType.Default
        |> ignore)

[<Test>]
let ``T.Options.WindLayers.WindLayers_ValueType can be constructed``() =
    Assert.DoesNotThrow(fun () ->
        T.Options.WindLayers.WindLayers_ValueType.Create(T.Integer.N 0, T.Integer.N 1, T.Integer.N 2)
        |> ignore)

[<Test>]
// When compiling with 6.0.0 exhibits the following compile-time error:
// The type provider 'SturmovikMission.DataProvider.TypeProvider.MissionTypes' reported an error in the context of provided type 'SturmovikMissionTypes.Provider,sample="..\\..\\..\\data\\Sample.Mission"+GroupData', member 'CreateMcuList'. The error: Type mismatch when building 'expr': the expression has the wrong type. Expected 'Microsoft.FSharp.Collections.FSharpList`1[SturmovikMission.DataProvider.Ast+Data]', but received type 'Microsoft.FSharp.Collections.FSharpList`1[SturmovikMission.DataProvider.Ast+Data]'.Parameter name: receivedType
let ``T.GroupData.CreateMcuList does not create a compile-time type mismatch``() =
    let timer =
        T.MCU_Timer.Default.SetIndex(T.Integer.N 1)
    let data = sprintf "Group {\n  Name = \"G\";\n%s\n }" (timer.AsString())
    let s = Parsing.Stream.FromString data
    let groupData = T.GroupData.Parse(s)
    Assert.DoesNotThrow(fun () -> groupData.CreateMcuList() |> ignore)

[<Test>]
let ``T.Boolean default value can be retrieved``() =
    Assert.DoesNotThrow(fun () -> let x : T.Boolean = T.Boolean.Default in ())

[<Test>]
let ``T.MCU_TR_ComplexTrigger has its type properly properly inferred``() =
    let vt = T.MCU_TR_ComplexTrigger.AstType
    printfn "%A" vt
    match vt with
    | Ast.ValueType.Composite fields ->
        Assert.IsTrue(fields.ContainsKey "Enabled")
        match fields.TryFind "Enabled" with
        | Some(ft, multMin, multMax) ->
            Assert.AreEqual(Ast.ValueType.Boolean, ft)
            Assert.AreEqual(Ast.MinMultiplicity.MinOne, multMin)
            Assert.AreEqual(Ast.MaxMultiplicity.MaxOne, multMax)
        | None ->
            ()

        Assert.IsTrue(fields.ContainsKey "OnEvents")
        match fields.TryFind "OnEvents" with
        | Some (ft, multMin, multMax) ->
            match ft with
            | Ast.ValueType.Composite oneField ->
                Assert.AreEqual(1, oneField.Count)
                match oneField.TryFind "OnEvent" with
                | Some (_, multMin, multMax) ->
                    Assert.AreEqual(Ast.MinMultiplicity.MinOne, multMin, "OnEvent should appear at least once in OnEvents")
                    Assert.AreEqual(Ast.MaxMultiplicity.Multiple, multMax, "OnEvent should be allowed to appear multiple times")
                | None ->
                    Assert.Fail("OneEvents lacks field OnEvent")
            | _ ->
                Assert.Fail("OnEvents is not a composite")
            Assert.AreEqual(Ast.MinMultiplicity.Zero, multMin, "OnEvents should be optional")
            Assert.AreEqual(Ast.MaxMultiplicity.MaxOne, multMax, "OnEvents should appear at most once")
        | None ->
            Assert.Fail("Component OnEvents is missing")
    | _ ->
        Assert.Fail("Complex trigger MCU is not a composite")

[<Test>]
let ``T.MCU_TR_ComplexTrigger can be converted to MCU, dumped, and parsed again``() =
    let complex = T.MCU_TR_ComplexTrigger.Default
    let repr = complex.AsString()
    let mcu = complex.CreateMcu()
    let repr2 = mcu.AsString()
    printfn "%s" repr
    Assert.AreEqual(repr, repr2)
    let parser = T.MCU_TR_ComplexTrigger.GetParser()
    let complex2, _ =
        try
            parser.Run(Parsing.Stream.FromString repr)
        with
        | :? Parsing.ParseError as err ->
            Parsing.printParseError err
            |> String.concat "\n"
            |> eprintfn "%s"
            failwith "Unexpected parse error"
    Assert.AreEqual(complex.Wrapped, complex2)

[<Test>]
let ``T.MCU_TR_ComplexTrigger with events can be dumped and parsed again``() =
    let onEvents =
        T.MCU_TR_ComplexTrigger.OnEvents.Default.SetOnEvent[T.MCU_TR_ComplexTrigger.OnEvents.OnEvent.Default]
    let complex =
        T.MCU_TR_ComplexTrigger.Default
            .SetOnEvents(Some onEvents)
    let repr = complex.AsString()
    printfn "%s" repr
    let parser = T.MCU_TR_ComplexTrigger.GetParser()
    let complex2, _ =
        try
            parser.Run(Parsing.Stream.FromString repr)
        with
        | :? Parsing.ParseError as err ->
            Parsing.printParseError err
            |> String.concat "\n"
            |> eprintfn "%s"
            failwith "Unexpected parse error"
    Assert.AreEqual(complex.Wrapped, complex2)

[<Test>]
let ``T.GroupData can create lists of MCUs``() =
    let timers =
        [
            T.MCU_Timer.Default.SetIndex(T.Integer.N 1)
            T.MCU_Timer.Default.SetIndex(T.Integer.N 2)
            T.MCU_Timer.Default.SetIndex(T.Integer.N 3)
        ]
    let values =
        timers
        |> List.map (fun timer -> Ast.Data.Leaf("MCU_Timer", timer.Wrapped))
    let group = T.GroupData(values)
    Assert.DoesNotThrow(fun () -> group.CreateMcuList() |> ignore)
    let mcus = group.CreateMcuList()
    Assert.AreEqual(timers.Length, mcus.Length, "List of MCUs should contain the correct number of elements")
    for mcu in mcus do
        printfn "%s" (mcu.AsString())
    let timers2 = group.ListOfMCU_Timer
    Assert.AreEqual(timers.Length, timers2.Length, "ListOfMCU_Timer should contain the correct number of elements")

[<Test>]
let ``T.GroupData can parse files``() =
    let timers =
        [
            T.MCU_Timer.Default.SetIndex(T.Integer.N 1)
            T.MCU_Timer.Default.SetIndex(T.Integer.N 2)
            T.MCU_Timer.Default.SetIndex(T.Integer.N 3)
        ]
    let repr =
        timers
        |> Seq.map (fun timer -> timer.AsString())
        |> String.concat "\n"
    let s = Parsing.Stream.FromString repr
    Assert.DoesNotThrow(fun () -> T.GroupData.Parse(s) |> ignore)
    let group = T.GroupData.Parse(s)
    let mcus = group.CreateMcuList()
    Assert.AreEqual(timers.Length, mcus.Length, "List of MCUs should contain the correct number of elements")
    for mcu in mcus do
        printfn "%s" (mcu.AsString())
    let timers2 = group.ListOfMCU_Timer
    Assert.AreEqual(timers.Length, timers2.Length, "ListOfMCU_Timer should contain the correct number of elements")

[<Test>]
let ``T.Airfield has functional getters``() =
    let planesList =
        [
            T.Airfield.Planes.Plane.Default
        ]
    let planes = T.Airfield.Planes.Default.SetPlane(planesList)
    let outPlanes = planes.GetPlanes()
    Assert.AreEqual(planesList.Head.Wrapped, Seq.head(outPlanes).Wrapped)
    let airfield = T.Airfield.Default.SetPlanes(planes)
    let read = airfield.GetPlanes()
    Assert.AreEqual(planes.Wrapped, read.Wrapped)

[<Test>]
let ``T.Train has a functional optional getter for carriages``() =
    let carriages = T.Train.Carriages.FromList [T.String.N "carriage1"; T.String.N "carriage2"]
    let train = T.Train.Default.SetCarriages(Some carriages)
    let carriages2 = train.TryGetCarriages()
    Assert.AreEqual(Some(carriages.Wrapped), carriages2 |> Option.map (fun x -> x.Wrapped))

[<Test>]
let ``T.Options has a functional getter for lists``() =
    let configs = [T.String.N "A"; T.String.N "B"]
    let options = T.Options.Default.SetMultiplayerPlaneConfig(configs)
    let configsOut =
        options.GetMultiplayerPlaneConfigs()
        |> List.ofSeq
    let unwrap = List.map (fun (x : T.String) -> x.Wrapped)
    Assert.AreEqual(unwrap configs, unwrap configsOut)

[<Test>]
let ``T.Block.Damaged has a functional getter for maps``() =
    let damages =
        [ 0, T.Float.N 0.5
          123, T.Float.N 1.0
        ] |> Map.ofList
    let damaged = T.Block.Damaged.FromMap(damages)
    let damagesOut = damaged.Value
    let unwrap = Map.map (fun _ (v : T.Float) -> v.Value)
    Assert.AreEqual(unwrap damages, unwrap damagesOut)