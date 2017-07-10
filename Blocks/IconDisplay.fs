﻿module SturmovikMission.Blocks.IconDisplay

open SturmovikMission.DataProvider
open System.Numerics
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.DataProvider.McuUtil
open Vector

type IconDisplay = {
    Show : Mcu.McuTimer
    Hide : Mcu.McuTrigger
    Icon : Mcu.McuIcon
    All : McuUtil.IMcuGroup
}
with
    /// <summary>
    /// Create an icon that can be shown or hidden
    /// </summary>
    /// <param name="pos">Location of the icon</param>
    /// <param name="label">Label of the icon</param>
    /// <param name="coalition">Coalition which can see the icon</param>
    /// <param name="iconType">The type of the icon</param>
    static member Create(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, pos : Vector2, label : string, coalition : Mcu.CoalitionValue, iconType : Mcu.IconIdValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let substLc = Mcu.substLCId <| lcStore.GetIdMapper()
        let group = blocksData.GetGroup("Icon").CreateMcuList()
        for mcu in group do
            subst mcu
            substLc mcu
        // Get key nodes
        let show = getTriggerByName group T.Blocks.Show :?> Mcu.McuTimer
        let hide = getTriggerByName group T.Blocks.Hide
        let activate = getByIndex show.Targets.Head group :?> Mcu.McuTrigger
        let icon = getIconByIndex activate.Targets.Head group
        // Position of all nodes
        let refPos = Vector2.FromMcu icon.Pos
        let dv = pos - refPos
        for mcu in group do
            (Vector2.FromMcu mcu.Pos + dv).AssignTo(mcu.Pos)
        // Icon type
        icon.Coalitions <- [ coalition ]
        icon.IconId <- iconType
        { Show = show
          Hide = hide
          Icon = icon
          All =
            { new McuUtil.IMcuGroup with
                member x.Content = group
                member x.LcStrings = [ icon.IconLC.Value.LCName, label ]
                member x.SubGroups = []
            }
        }

    /// <summary>
    /// Create a pair of colocated icons, visible to each coalition as attack/cover
    /// </summary>
    /// <param name="coalition">The coalition with the attacking role</param>
    /// <param name="iconType">The icon type, should be of one of the CoverXXX types</param>
    static member CreatePair(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, pos : Vector2, label : string, coalition : Mcu.CoalitionValue, iconType : Mcu.IconIdValue) =
        let one = IconDisplay.Create(store, lcStore, pos, label, coalition, iconType)
        let other =
            match coalition with
            | Mcu.CoalitionValue.Allies -> Mcu.CoalitionValue.Axis
            | Mcu.CoalitionValue.Axis -> Mcu.CoalitionValue.Allies
            | _ -> invalidArg "coalition" "Must be Axis or Allies"
        let two = IconDisplay.Create(store, lcStore, pos, label, other, enum((int iconType) - 50))
        one, two