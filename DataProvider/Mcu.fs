﻿module SturmovikMission.DataProvider.Mcu

open SturmovikMission.DataProvider.Ast

/// <summary>
/// 3d vector type used for positions and orientations.
/// </summary>
type Vec3 =
    abstract X : float with get, set
    abstract Y : float with get, set
    abstract Z : float with get, set

/// <summary>
/// Localization data for icons.
/// </summary>
type IconLCData =
    abstract LCName : int with get, set
    abstract LCDesc : int with get, set

/// <summary>
/// Localization data for subtitles.
/// </summary>
type SubtitleLCData =
    abstract LCText : int with get, set

/// <summary>
/// Base interface for all MCUs.
/// A subset of the properties of objects is made accessible. These are the
/// properties that would typically need to be changed when instancing a
/// template.
/// Instances of this type are mutable.
/// </summary>
type McuBase =
    abstract Index : int with get, set
    abstract Name : string with get, set
    abstract Pos : Vec3
    abstract Ori : Vec3
    abstract IconLC : IconLCData option
    abstract SubtitleLC : SubtitleLCData option
    /// <summary>
    /// Build a string using the syntax of mission files that specifies all the
    /// fields of this instance (not only those in McuBase and its subtypes).
    /// </summary>
    abstract AsString : unit -> string

/// <summary>
/// Interface of commands (timers, proximity triggers...)
/// </summary>
type McuCommand =
    inherit McuBase
    abstract Objects : int list with get, set
    abstract Targets : int list with get, set

/// <summary>
/// Connection of an event from an entity to a target command.
/// </summary>
type EventConnection =
    { Type : int
      TarId : int }

/// <summary>
/// Interface of entities, i.e. active parts of vehicles and other 3d objects.
/// </summary>
type McuEntity =
    inherit McuCommand
    abstract MisObjID : int with get, set
    abstract OnEvents : EventConnection list with get, set

/// <summary>
/// Interface of things that have entities: ground vehicles, planes, artillery, buildings, bridges...
/// </summary>
type HasEntity =
    inherit McuBase
    /// <summary>
    /// Get or set the link to the entity. Set to 0 it lacks an entity.
    /// </summary>
    abstract LinkTrId : int with get, set

/// <summary>
/// Substitute occurrences of numerical ids in an MCU.
/// </summary>
/// <param name="getNewId">Function that provides the new id given an old id.</param>
/// <param name="mcu">The MCU whose ids are changed. Instance is mutated.</param>
let substId (getNewId : int -> int) (mcu : McuBase) =
    mcu.Index <- getNewId mcu.Index
    match mcu with
    | :? McuCommand as cmd ->
        cmd.Objects <- cmd.Objects |> List.map getNewId
        cmd.Targets <- cmd.Targets |> List.map getNewId
    | _ -> ()
    match mcu with
    | :? McuEntity as ent ->
        ent.MisObjID <- getNewId ent.MisObjID
        ent.OnEvents <- ent.OnEvents |> List.map (fun ev -> { ev with TarId = getNewId ev.TarId })
    | _ -> ()
    match mcu with
    | :? HasEntity as veh ->
        veh.LinkTrId <- getNewId veh.LinkTrId
    | _ -> ()

/// <summary>
/// Substitute occurrences of numerical ids of localization strings in an MCU
/// </summary>
/// <param name="getNewId">Function that provides the new id given an old id.</param>
/// <param name="mcu">The MCU whose localization string ids are changed. Instance is mutated.</param>
let substLCId (getNewId : int -> int) (mcu : McuBase) =
    match mcu.IconLC with
    | Some op ->
        op.LCDesc <- getNewId op.LCDesc
        op.LCName <- getNewId op.LCName
    | None -> ()
    match mcu.SubtitleLC with
    | Some op ->
        op.LCText <- getNewId op.LCText
    | None -> ()

/// <summary>
/// Add an object link to a command.
/// </summary>
/// <param name="mcu">The command, which is mutated.</param>
/// <param name="objekt">The id of the object.</param>
let addObjectLink (mcu : McuCommand) (objekt : int) =
    mcu.Objects <- objekt :: mcu.Objects

/// <summary>
/// Add a target link to a command.
/// </summary>
/// <param name="mcu">The command, which is mutated.</param>
/// <param name="target">The id of the target.</param>
let addTargetLink (mcu : McuCommand) (target : int) =
    mcu.Targets <- target :: mcu.Targets

/// <summary>
/// Connect a entity-holding object and its entity.
/// </summary>
/// <param name="veh">The entity owner, which is mutated.</param>
/// <param name="ent">The entity, which is mutated.</param>
let connectEntity (veh : HasEntity) (ent : McuEntity) =
    veh.LinkTrId <- ent.Index
    ent.MisObjID <- veh.Index

