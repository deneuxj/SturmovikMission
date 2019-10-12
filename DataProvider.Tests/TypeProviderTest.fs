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
open Xunit

type T = Provider< @"..\data\Sample.Mission", "" >

[<Fact>]
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
    try
        parser.Parse_Vehicle(Parsing.Stream.FromString repr) |> ignore
        true
    with _ -> false

[<Fact>]
let ``airfield planes can be constructed``() =
    try
        let plane =
            T.Airfield.Planes()
        true
    with _ -> false

[<Fact>]
let ``wind layer value types can be default-constructed``() =
    try
        let windLayer = T.Options.WindLayers.WindLayers_ValueType()
        true
    with _ -> false

[<Fact>]
let ``wind layer value types can be constructed``() =
    try
        let windLayer = T.Options.WindLayers.WindLayers_ValueType((T.Integer 0, T.Integer 1, T.Integer 2))
        true
    with _ -> false