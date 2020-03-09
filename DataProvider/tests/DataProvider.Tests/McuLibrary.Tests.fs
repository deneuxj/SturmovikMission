//    Copyright 2020 Johann Deneux
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

module SturmovikMission.McuLibrary.Test

open NUnit.Framework
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.Ast
open SturmovikMission.DataProvider.AutoSchema
open SturmovikMission.DataProvider.Unification

[<Test>]
let ``Unification of optional fields in composite types is correct``() =
    let txt = """
MCU_TR_ComplexTrigger
{
  Index = 23;
  Name = "Translator Complex Trigger";
  Desc = "";
  Targets = [];
  Objects = [];
  XPos = 26808.398;
  YPos = 117.593;
  ZPos = 24061.435;
  XOri = 0.00;
  YOri = 0.00;
  ZOri = 0.00;
  Enabled = 1;
  Cylinder = 1;
  Radius = 1000;
  DamageThreshold = 1;
  DamageReport = 50;
  CheckVehicles = 0;
  CheckPlanes = 0;
  EventsFilterSpawned = 0;
  EventsFilterEnteredSimple = 0;
  EventsFilterEnteredAlive = 0;
  EventsFilterLeftSimple = 0;
  EventsFilterLeftAlive = 0;
  EventsFilterFinishedSimple = 0;
  EventsFilterFinishedAlive = 0;
  EventsFilterStationaryAndAlive = 0;
  EventsFilterFinishedStationaryAndAlive = 0;
  EventsFilterTookOff = 0;
  EventsFilterDamaged = 0;
  EventsFilterCriticallyDamaged = 0;
  EventsFilterRepaired = 0;
  EventsFilterKilled = 0;
  EventsFilterDropedBombs = 0;
  EventsFilterFiredFlare = 0;
  EventsFilterFiredRockets = 0;
  EventsFilterDroppedCargoContainers = 0;
  EventsFilterDeliveredCargo = 0;
  EventsFilterParatrooperJumped = 0;
  EventsFilterParatrooperLandedAlive = 0;
  Country = 202;
  Country = 0;
  ObjectScript = "luascripts\worldobjects\Planes\he111h6-c3.txt";
  ObjectScript = "luascripts\worldobjects\Planes\he111h6-c3-wm-low.txt";
  ObjectName = "df";
  ObjectName = "werwed";
  OnEvents
  {
    OnEvent
    {
      Type = 57;
      TarId = 8;
    }
    OnEvent
    {
      Type = 66;
      TarId = 19;
    }
  }
}

MCU_TR_ComplexTrigger
{
  Index = 111;
  Name = "Translator Complex Trigger";
  Desc = "";
  Targets = [];
  Objects = [];
  XPos = 24898.929;
  YPos = 119.718;
  ZPos = 23469.449;
  XOri = 0.00;
  YOri = 0.00;
  ZOri = 0.00;
  Enabled = 1;
  Cylinder = 1;
  Radius = 1000;
  DamageThreshold = 1;
  DamageReport = 50;
  CheckVehicles = 0;
  CheckPlanes = 0;
  EventsFilterSpawned = 0;
  EventsFilterEnteredSimple = 0;
  EventsFilterEnteredAlive = 0;
  EventsFilterLeftSimple = 0;
  EventsFilterLeftAlive = 0;
  EventsFilterFinishedSimple = 0;
  EventsFilterFinishedAlive = 0;
  EventsFilterStationaryAndAlive = 0;
  EventsFilterFinishedStationaryAndAlive = 0;
  EventsFilterTookOff = 0;
  EventsFilterDamaged = 0;
  EventsFilterCriticallyDamaged = 0;
  EventsFilterRepaired = 0;
  EventsFilterKilled = 0;
  EventsFilterDropedBombs = 0;
  EventsFilterFiredFlare = 0;
  EventsFilterFiredRockets = 0;
  EventsFilterDroppedCargoContainers = 0;
  EventsFilterDeliveredCargo = 0;
  EventsFilterParatrooperJumped = 0;
  EventsFilterParatrooperLandedAlive = 0;
}

"""
    let s = Parsing.Stream.FromString txt
    let types, _ = getTopTypes s
    let name, typ = types |> Map.toSeq |> Seq.head
    Assert.AreEqual("MCU_TR_ComplexTrigger", name, "Name of complex trigger should be MCU_TR...")
    match typ with
    | ValueType.Composite fields ->
        match fields.TryFind "OnEvents" with
        | Some(_, mmin, mmax) ->
            Assert.AreEqual(Zero, mmin, "Minimum multiplicity of OnEvents should be 0")
            Assert.AreEqual(MaxOne, mmax, "Maximum multiplicity of OnEvents should be 1")
        | None ->
            Assert.Fail("Composite should have a field named 'OnEvents'")
    | _ ->
        Assert.Fail("Inferred type should be composite")
    ()