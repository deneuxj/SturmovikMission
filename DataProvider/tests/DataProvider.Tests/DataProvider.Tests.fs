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

module SturmovikMission.DataProvider.McuLibraryTest

open SturmovikMissionTypes
open NUnit.Framework

type T = Provider< @"..\..\..\data\Sample.Mission">

[<Test>]
let ``vehicles have repair, refuel``() =
    let vehicle =
        T.Vehicle.Default
            .SetMaintenanceRadius(T.Integer 500)
            .SetRepairFriendlies(T.Boolean true)
            .SetRepairTime(T.Integer 60)
            .SetRefuelFriendlies(T.Boolean true)
            .SetRefuelTime(T.Integer 30)
    let repr = vehicle.AsString()
    let parser = T.Vehicle.GetParser()
    parser.Run(Parsing.Stream.FromString repr) |> ignore
    Assert.DoesNotThrow(fun () ->
        parser.Run(Parsing.Stream.FromString repr)
        |> ignore)

[<Test>]
let ``airfield planes can be constructed``() =
    Assert.DoesNotThrow(fun () ->
        T.Airfield.Planes.Default
        |> ignore)

[<Test>]
let ``wind layer value types can be default-constructed``() =
    Assert.DoesNotThrow(fun () ->
        T.Options.WindLayers.WindLayers_ValueType.Default
        |> ignore)

[<Test>]
let ``wind layer value types can be constructed``() =
    Assert.DoesNotThrow(fun () ->
        T.Options.WindLayers.WindLayers_ValueType.Create(T.Integer 0, T.Integer 1, T.Integer 2)
        |> ignore)

[<Test>]
// When compiling with 6.0.0 exhibits the following compile-time error:
// The type provider 'SturmovikMission.DataProvider.TypeProvider.MissionTypes' reported an error in the context of provided type 'SturmovikMissionTypes.Provider,sample="..\\..\\..\\data\\Sample.Mission"+GroupData', member 'CreateMcuList'. The error: Type mismatch when building 'expr': the expression has the wrong type. Expected 'Microsoft.FSharp.Collections.FSharpList`1[SturmovikMission.DataProvider.Ast+Data]', but received type 'Microsoft.FSharp.Collections.FSharpList`1[SturmovikMission.DataProvider.Ast+Data]'.Parameter name: receivedType
let ``GroupData.CreateMcuList does not create a compile-time type mismatch``() =
    let timer =
        T.MCU_Timer.Default.SetIndex(T.Integer 1)
    let data = sprintf "Group {\n  Name = \"G\";\n%s\n }" (timer.AsString())
    let s = Parsing.Stream.FromString data
    let groupData = T.GroupData.Parse(s)
    Assert.DoesNotThrow(fun () -> groupData.CreateMcuList() |> ignore)
