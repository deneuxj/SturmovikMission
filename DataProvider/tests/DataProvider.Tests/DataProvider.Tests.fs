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
        T.Vehicle()
            .SetMaintenanceRadius(T.Integer 500)
            .SetRepairFriendlies(T.Boolean true)
            .SetRepairTime(T.Integer 60)
            .SetRefuelFriendlies(T.Boolean true)
            .SetRefuelTime(T.Integer 30)
    let repr = vehicle.AsString()
    let parser = T.Parser()
    Assert.Throws<Parsing.ParseError>(fun () ->
        parser.Parse_Vehicle(Parsing.Stream.FromString repr)
        |> ignore)
    |> ignore
    parser.Parse_Vehicle_1(Parsing.Stream.FromString repr) |> ignore
    Assert.DoesNotThrow(fun () ->
        parser.Parse_Vehicle_1(Parsing.Stream.FromString repr)
        |> ignore)

[<Test>]
let ``airfield planes can be constructed``() =
    Assert.DoesNotThrow(fun () ->
        T.Airfield.Planes()
        |> ignore)

[<Test>]
let ``wind layer value types can be default-constructed``() =
    Assert.DoesNotThrow(fun () ->
        T.Options.WindLayers.WindLayers_ValueType()
        |> ignore)

[<Test>]
let ``wind layer value types can be constructed``() =
    Assert.DoesNotThrow(fun () ->
        T.Options.WindLayers.WindLayers_ValueType((T.Integer 0, T.Integer 1, T.Integer 2))
        |> ignore)
