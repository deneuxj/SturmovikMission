﻿/// Given a an old world state and mission result data, produce an updated world state
module Campaign.NewWorldState

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.ResultExtraction

/// What to produce in each category of production, and how much does each category need
type ProductionPriorities = {
    Vehicle : GroundAttackVehicle
    PriorityVehicle : float32<E>
    Plane : PlaneModel
    PriorityPlane : float32<E>
    PriorityShells : float32<E>
}

/// Decide what vehicles and planes to produce, and how important they are.
let computeProductionPriorities (coalition : CoalitionId) (world : World) (state : WorldState) =
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state

    let shellNeed =
        state.Regions
        |> Seq.sumBy (fun state ->
            if state.Owner = Some coalition then
                let capacity =
                    let region = wg.GetRegion state.RegionId
                    Seq.zip region.Storage state.StorageHealth
                    |> Seq.sumBy (fun (sto, health) -> health * getShellsPerBuilding sto.Model)
                capacity - state.ShellCount
                |> max 0.0f
            else
                0.0f)
        |> (*) shellCost
    
    let vehicleToProduce, vehicleNeed =
        let numHeavy, numMedium, numLight, need =
            let perRegion = GroundAttackVehicle.MediumTankCost * 10.0f
            state.Regions
            |> Seq.filter (fun state -> state.Owner = Some coalition) // Regions we control
            |> Seq.filter (fun state -> // Regions at the front
                let region = wg.GetRegion state.RegionId
                region.Neighbours
                |> Seq.exists (fun ngh -> sg.GetRegion(ngh).Owner <> Some coalition))
            |> Seq.map (fun state ->
                let numHeavy = state.GetNumVehicles HeavyTank
                let numMedium = state.GetNumVehicles MediumTank
                let numLight = state.GetNumVehicles LightArmor
                let desiredValue = 3.0f * GroundAttackVehicle.HeavyTankCost + 9.0f * GroundAttackVehicle.MediumTankCost + 3.0f * GroundAttackVehicle.LightArmorCost
                let availableValue = float32 numHeavy * GroundAttackVehicle.HeavyTankCost + float32 numMedium * GroundAttackVehicle.MediumTankCost + float32 numLight * GroundAttackVehicle.LightArmorCost
                numHeavy, numMedium, numLight, desiredValue - availableValue)
            |> Seq.fold (fun (t1, t2, t3, t4) (n1, n2, n3, n4) -> (t1 + n1, t2 + n2, t3 + n3, t4 + n4)) (0, 0, 0, 0.0f<E>)
        let vehicle =
            if numMedium = 0 then
                MediumTank
            elif numHeavy < numLight && 3 * numHeavy < numMedium then
                HeavyTank
            elif numLight >= numHeavy && 3 * numLight < numMedium then
                LightArmor
            else
                MediumTank
        vehicle, need

    let planeNeed(af : Airfield, state : AirfieldState) =
        let bomberCapacity =
            af.ParkedBombers |> List.length |> float32
        let attackerCapacity =
            af.ParkedAttackers |> List.length |> float32
        let fighterCapacity =
            af.ParkedFighters |> List.length |> float32
        let numPlanesOfType models =
            models
            |> List.choose (fun model -> Map.tryFind model state.NumPlanes)
            |> List.sum
            |> float32
        let numBombers = numPlanesOfType [ PlaneModel.Ju88a4; Pe2s35 ]
        let numTransports = numPlanesOfType [ PlaneModel.Ju52 ]
        let numAttackPlanes = numPlanesOfType [ PlaneModel.Bf110e; IL2M41 ]
        let numFighters = numPlanesOfType [ PlaneModel.Bf109e7; Bf109f2; Mc202; I16; Mig3; P40 ]
        let fighterUsage =
            if fighterCapacity > 0.0f then
                numFighters / fighterCapacity
            else
                1.0f
        let planeCost (plane : PlaneModel) =
            let numPlanesNeeded =
                match plane with
                | Bf109e7
                | Bf109f2
                | Mc202
                | I16
                | Mig3
                | P40 -> fighterCapacity - numFighters
                | IL2M41
                | Bf110e -> attackerCapacity - numAttackPlanes
                | _ -> bomberCapacity - numBombers - numTransports
            plane, plane.Cost * numPlanesNeeded
        if fighterUsage < 0.5f then
            match coalition with
            | Axis -> Bf109e7
            | Allies -> P40
        elif fighterUsage < 0.75f then
            match coalition with
            | Axis -> Mc202
            | Allies -> I16
        elif numAttackPlanes < attackerCapacity then
            match coalition with
            | Axis -> Bf110e
            | Allies -> IL2M41
        elif numBombers < bomberCapacity then
            match coalition with
            | Axis ->
                if numBombers / bomberCapacity > 0.75f then
                    Ju52
                else
                    Ju88a4
            | Allies ->
                Pe2s35
        elif fighterUsage < 1.0f then
            match coalition with
            | Axis -> Bf109f2
            | Allies -> Mig3
        else
            match coalition with
            | Axis -> Bf109e7
            | Allies -> I16
        |> planeCost
    let plane, planeNeed =
        try
            world.Airfields
            |> Seq.map (fun af -> af, sg.GetAirfield af.AirfieldId)
            |> Seq.filter (fun (af, afs) -> sg.GetRegion(af.Region).Owner = Some coalition)
            |> Seq.map planeNeed
            |> Seq.groupBy fst
            |> Seq.map (fun (plane, needs) -> plane, needs |> Seq.sumBy snd)
            |> Seq.maxBy snd
        with
        | _ ->
            match coalition with
            | Axis -> Bf109e7, 0.0f<E>
            | Allies -> I16, 0.0f<E>
    { Vehicle = vehicleToProduce
      PriorityVehicle = vehicleNeed
      Plane = plane
      PriorityPlane = planeNeed
      PriorityShells = shellNeed
    }


/// Compute the total production capacity of a coalition.
let computeProduction (coalition : CoalitionId) (region : Region) (regState : RegionState) =
    Seq.zip region.Production regState.ProductionHealth
    |> Seq.sumBy (fun (prod, health) -> health * getProductionPerBuilding prod.Model)

/// Add production units according to production priorities
let applyProduction (dt : float32<H>) (world : World) (state : WorldState) =
    let wg = WorldFastAccess.Create world
    let work (state : WorldState) (coalition : CoalitionId) =
        let priorities = computeProductionPriorities coalition world state
        let vehiclePrio, planePrio, energyPrio =
            let total =
                priorities.PriorityPlane + priorities.PriorityShells + priorities.PriorityVehicle
                |> max 0.1f<E>
            priorities.PriorityVehicle / total, priorities.PriorityPlane / total, priorities.PriorityVehicle / total
        let regions =
            [
                for region, regState in Seq.zip world.Regions state.Regions do
                    if regState.Owner = Some coalition then
                        let energy = dt * computeProduction coalition region regState
                        let shells = regState.Products.Supplies + energyPrio * energy
                        let planes =
                            let oldValue =
                                match Map.tryFind priorities.Plane regState.Products.Planes with
                                | Some x -> x
                                | None -> 0.0f<E>
                            let newValue = oldValue + planePrio * energy
                            Map.add priorities.Plane newValue regState.Products.Planes
                        let vehicles =
                            let oldValue =
                                match Map.tryFind priorities.Vehicle regState.Products.Vehicles with
                                | Some x -> x
                                | None -> 0.0f<E>
                            let newValue = oldValue + vehiclePrio * energy
                            Map.add priorities.Vehicle newValue regState.Products.Vehicles
                        let assignment = { regState.Products with Supplies = shells; Planes = planes; Vehicles = vehicles }
                        yield { regState with Products = assignment }
                    else
                        yield regState
            ]
        { state with Regions = regions }
    [ Axis; Allies ]
    |> List.fold work state

/// Create new plane and vehicle instances if enough energy has been accumulated and convert supplies to local Resupplied events.
let convertProduction (world : World) (state : WorldState) =
    let afStates =
        state.Airfields
        |> Seq.map (fun af -> af.AirfieldId, af)
        |> Map.ofSeq
        |> ref
    let regions =
        state.Regions
        |> Seq.map (fun region -> region.RegionId, region)
        |> Map.ofSeq
        |> ref
    let resupplied = ref []
    for region, regState in Seq.zip world.Regions state.Regions do
        let newPlanes, remainingPlanes =
            regState.Products.Planes
            |> Map.toSeq
            |> Seq.fold (fun (newPlanes, remainingPlanes) (plane, energy) ->
                let numNewPlanes = floor(energy / plane.Cost)
                let energyLeft = energy - numNewPlanes * energy
                (plane, int numNewPlanes) :: newPlanes, Map.add plane energy remainingPlanes
            ) ([], Map.empty)
        let newVehicles, remainingVehicles =
            regState.Products.Vehicles
            |> Map.toSeq
            |> Seq.fold (fun (newVehicles, remainingVehicles) (vehicle, energy) ->
                let numNewVehicles = floor(energy / vehicle.Cost)
                let energyLeft = energy - numNewVehicles * energy
                (vehicle, int numNewVehicles) :: newVehicles, Map.add vehicle energy remainingVehicles
            ) ([], Map.empty)
        let supplySpaceAvailable =
            Seq.zip region.Storage regState.StorageHealth
            |> Seq.sumBy (fun (sto, health) -> health * getEnergyHealthPerBuilding sto.Model)
            |> fun x -> x - regState.ShellCount * shellCost
            |> max 0.0f<E>
        let transferedEnergy = min supplySpaceAvailable regState.Products.Supplies
        resupplied := { Region = region.RegionId; Energy = transferedEnergy} :: !resupplied
        let suppliesLeft = regState.Products.Supplies - transferedEnergy
        let assignment = { regState.Products with Planes = remainingPlanes; Vehicles = remainingVehicles; Supplies = suppliesLeft }
        regions := Map.add regState.RegionId { regState with Products = assignment } !regions
    { state with
        Airfields = state.Airfields |> List.map (fun af -> afStates.Value.[af.AirfieldId])
        Regions = state.Regions |> List.map (fun region -> regions.Value.[region.RegionId]) },
    resupplied.Value

/// Apply damages due to attacks, use supplies to repair damages and replenish storage
let applyRepairsAndDamages (dt : float32<H>) (world : World) (state : WorldState) (supplies : Resupplied list) (damages : Damage list) =
    let wg = WorldFastAccess.Create world
    let damages =
        let data =
            damages
            |> Seq.groupBy (fun dmg -> dmg.Object)
            |> Seq.map (fun (victim, damages) -> victim, damages |> Seq.map (fun dmg -> dmg.Data))
        Map.ofSeq data
    let supplies =
        let data =
            supplies
            |> Seq.groupBy (fun sup -> sup.Region)
            |> Seq.map (fun (reg, sups) -> reg, sups |> Seq.sumBy (fun sup -> sup.Energy))
        Map.ofSeq data
    let regionsAfterDamages =
        [
            for regState in state.Regions do
                let region = wg.GetRegion regState.RegionId
                let prodHealth =
                    regState.ProductionHealth
                    |> List.mapi (fun idx health ->
                        match Map.tryFind (Production(region.RegionId, idx)) damages with
                        | Some damages ->
                            damages
                            |> Seq.sumBy (fun data -> data.Amount)
                            |> (-) health
                        | None ->
                            health)
                let storeHealth =
                    regState.StorageHealth
                    |> List.mapi (fun idx health ->
                        match Map.tryFind (Storage(region.RegionId, idx)) damages with
                        | Some damages ->
                            damages
                            |> Seq.sumBy (fun data -> data.Amount)
                            |> (-) health
                        | None ->
                            health)
                // TODO: canon losses
                yield { regState with ProductionHealth = prodHealth; StorageHealth = storeHealth}
        ]
    let regionsAfterSupplies =
        [
            for regState in regionsAfterDamages do
                let region = wg.GetRegion regState.RegionId
                let energy =
                    match Map.tryFind region.RegionId supplies with
                    | Some e -> e
                    | None -> 0.0f<E>
                let computeHealing(healths, buildings, energy) =
                    let energyPerBuilding =
                        buildings
                        |> List.map (fun (x : StaticGroup) -> getEnergyHealthPerBuilding x.Model)
                    let prodHealing, energy =
                        List.zip healths energyPerBuilding
                        |> List.fold (fun (healings, available) (health, healthCost : float32<E>) ->
                            let spend =
                                min ((1.0f - health) * healthCost) available
                            (spend / healthCost) :: healings, available - spend
                        ) ([], energy)
                    let prodHealing = List.rev prodHealing
                    let prodHealth =
                        List.zip healths prodHealing
                        |> List.map (fun (health, healing) -> health + healing)
                    prodHealth, energy
                let prodHealth, energy =
                    computeHealing(
                        regState.ProductionHealth,
                        region.Production,
                        energy)
                let storeHealth, energy =
                    computeHealing(
                        regState.StorageHealth,
                        region.Storage,
                        energy)
                let shellCount = regState.ShellCount + energy / shellCost
                yield { regState with ProductionHealth = prodHealth; StorageHealth = storeHealth; ShellCount = shellCount }
        ]
    { state with Regions = regionsAfterSupplies }

/// Update airfield planes according to departures and arrivals
let applyPlaneTransfers (state : WorldState) (takeOffs : TookOff list) (landings : Landed list) =
    let airfields =
        state.Airfields
        |> Seq.map (fun af -> af.AirfieldId, af)
        |> Map.ofSeq
    let airfieldsAfterTakeOffs =
        takeOffs
        |> List.fold (fun airfields takeOff ->
            let af = Map.find takeOff.Airfield airfields
            let oldPlaneValue =
                Map.tryFind takeOff.Plane af.NumPlanes
                |> fun x -> defaultArg x 0.0f
            let newPlanes =
                Map.add takeOff.Plane (oldPlaneValue - 1.0f) af.NumPlanes
            Map.add takeOff.Airfield { af with NumPlanes = newPlanes } airfields
        ) airfields
    let airfieldsAfterLandings =
        landings
        |> List.fold (fun airfields landing ->
            let af = Map.find landing.Airfield airfields
            let oldPlaneValue =
                Map.tryFind landing.Plane af.NumPlanes
                |> fun x -> defaultArg x 0.0f
            let newPlanes =
                Map.add landing.Plane (oldPlaneValue + landing.Health) af.NumPlanes
            Map.add landing.Airfield { af with NumPlanes = newPlanes} airfields
        ) airfieldsAfterTakeOffs
    let airfields =
        state.Airfields
        |> List.map (fun af -> airfieldsAfterLandings.[af.AirfieldId])
    { state with Airfields = airfields }


let eveningStop = 18
let morningStart = 8

let newState (dt : float32<H>) (world : World) (state : WorldState) supplies damages tookOff landed =
    let state = applyProduction dt world state
    let state, extra = convertProduction world state
    let state = applyRepairsAndDamages dt world state (supplies @ extra) damages
    let state = applyPlaneTransfers state tookOff landed
    let h = floor(float32 dt)
    let mins = 60.0f * (float32 dt) - h
    let newDate =
        let x = state.Date + System.TimeSpan(int h, int mins, 0)
        let extra =
            if x.Hour >= eveningStop then
                morningStart - eveningStop + 24
            else 0
        x + System.TimeSpan(extra, 0, 0)
    { state with Date = newDate }