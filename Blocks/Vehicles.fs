﻿// model/script strings of various known vehicles and planes
module SturmovikMission.Blocks.Vehicles

open SturmovikMission.DataProvider

type T = SturmovikMissionTypes.Provider<"../data/Blocks/Vehicles.mission", "../data/Blocks/Vehicles.mission">

type VehicleTypeData = {
    Script : string
    Model : string
}

let private data =
    T.GroupData(Parsing.Stream.FromFile "Vehicles.mission").ListOfVehicle
    |> List.map (fun vehicle -> vehicle.Name.Value, vehicle)
    |> dict

let russianCar =
    let truck = data.[T.Vehicles.CarRUS]
    { Script = truck.Script.Value
      Model = truck.Model.Value
    }

let germanCar =
    let truck = data.[T.Vehicles.CarGER]
    { Script = truck.Script.Value
      Model = truck.Model.Value
    }

let russianTruck =
    let truck = data.[T.Vehicles.TruckRUS]
    { Script = truck.Script.Value
      Model = truck.Model.Value
    }

let germanTruck =
    let truck = data.[T.Vehicles.TruckGER]
    { Script = truck.Script.Value
      Model = truck.Model.Value
    }

let russianAntiTankCanon =
    let canon = data.[T.Vehicles.AntiTankRUS]
    { Script = canon.Script.Value
      Model = canon.Model.Value
    }

let germanAntiTankCanon =
    let canon = data.[T.Vehicles.AntiTankGER]
    { Script = canon.Script.Value
      Model = canon.Model.Value
    }

let russianAntiAirCanon =
    let canon = data.[T.Vehicles.AntiAirRUS]
    { Script = canon.Script.Value
      Model = canon.Model.Value
    }

let germanAntiAirCanon =
    let canon = data.[T.Vehicles.AntiAirGER]
    { Script = canon.Script.Value
      Model = canon.Model.Value
    }

let russianFlak =
    let canon = data.[T.Vehicles.FlakRUS]
    { Script = canon.Script.Value
      Model = canon.Model.Value
    }

let germanFlak =
    let canon = data.[T.Vehicles.FlakGER]
    { Script = canon.Script.Value
      Model = canon.Model.Value
    }