//    Copyright 2015 Johann Deneux
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

module SturmovikMission.DataProvider.Unification

open SturmovikMission.DataProvider.Ast

exception UnificationFailure of string

let mkFailedUnification kind1 kind2 msg =
    sprintf "Failed to unify %A and %A (%s)" kind1 kind2 msg

let (|LikeInteger|_|) =
    function
    | ValueType.Boolean
    | ValueType.Integer
    | ValueType.Mask -> Some()
    | _ -> None

let rec tryUnify =
    function
    | kind1, kind2 when kind1 = kind2 ->
        Ok(kind1)
    | ValueType.Boolean, ValueType.Mask
    | ValueType.Mask, ValueType.Boolean ->
        Ok(ValueType.Mask)
    | ValueType.Boolean, ValueType.Integer
    | ValueType.Integer, ValueType.Boolean ->
        Ok(ValueType.Integer)
    | LikeInteger, ValueType.Float
    | ValueType.Float, LikeInteger ->
        Ok(ValueType.Float)
    | ValueType.Composite comp, ValueType.Mapping kind
    | ValueType.Mapping kind, ValueType.Composite comp ->
        tryUnifyMappingAndComposite kind comp
    | ValueType.Composite kinds1, ValueType.Composite kinds2 ->
        tryUnifyComposites kinds1 kinds2
    | ValueType.Mapping kind1, ValueType.Mapping kind2 ->
        match tryUnify(kind1, kind2) with
        | Ok kind ->
            Ok(ValueType.Mapping kind)
        | Error msg ->
            Error (mkFailedUnification kind1 kind2 msg)
    | ValueType.List kind1, ValueType.List kind2 ->
        match tryUnify(kind1, kind2) with
        | Ok kind ->
            Ok(ValueType.List kind)
        | Error msg ->
            Error (mkFailedUnification kind1 kind2 msg)
    | ValueType.Pair(kindA1, kindA2) as p1, (ValueType.Pair(kindB1, kindB2) as p2) ->
        match tryUnify(kindA1, kindB1), tryUnify(kindA2, kindB2) with
        | Ok kind1, Ok kind2 ->
            Ok(ValueType.Pair(kind1, kind2))
        | Error msg, _
        | _, Error msg ->
            Error (mkFailedUnification p1 p2 msg)
    | ValueType.Triplet(kindA1, kindA2, kindA3) as p1, (ValueType.Triplet(kindB1, kindB2, kindB3) as p2) ->
        match tryUnify(kindA1, kindB1), tryUnify(kindA2, kindB2), tryUnify(kindA3, kindB3) with
        | Ok kind1, Ok kind2, Ok kind3 ->
            Ok(ValueType.Triplet(kind1, kind2, kind3))
        | Error msg, _, _
        | _, Error msg, _
        | _, _, Error msg ->
            Error (mkFailedUnification p1 p2 msg)
    | kind1, kind2 ->
        Error (mkFailedUnification kind1 kind2 "Incompatible value types")

and tryUnifyMappingAndComposite kind comp =
    let unified =
        comp
        |> Map.fold (fun m k (v, multMin, multMax) ->
            match m, tryUnify(v, kind) with
            | Error _ as err, _ ->
                err
            | Ok m, Ok kind ->
                Map.add k (kind, multMin, multMax) m
                |> Ok
            | Ok m, Error msg ->
                Error(mkFailedUnification v kind msg)) (Ok Map.empty)
    match unified with
    | Ok m -> Ok(ValueType.Composite m)
    | Error msg -> Error msg

and tryUnifyComposites kinds1 kinds2 =
    let inOne = kinds1 |> Seq.map (fun kvp -> kvp.Key) |> Set.ofSeq
    let inTwo = kinds2 |> Seq.map (fun kvp -> kvp.Key) |> Set.ofSeq 
    let merged = ref Map.empty
    for label1 in Set.difference inOne inTwo do
        let (kind, _, maxMult) = kinds1.[label1]
        merged := Map.add label1 (kind, Zero, maxMult) !merged
    for label2 in Set.difference inTwo inOne do
        let (kind, _, maxMult) = kinds2.[label2]
        merged := Map.add label2 (kind, Zero, maxMult) !merged
    let rec work labels =
        match labels with
        | [] ->
            Ok(ValueType.Composite !merged)
        | label :: rest ->
            let (kind1, minMult1, maxMult1) = kinds1.[label]
            let (kind2, minMult2, maxMult2) = kinds2.[label]
            match tryUnify(kind1, kind2) with
            | Ok kind ->
                let minMult = least(minMult1, minMult2)
                let maxMult = most(maxMult1, maxMult2)
                merged := Map.add label (kind, minMult, maxMult) !merged
                work rest
            | Error msg ->
                Error(mkFailedUnification kind1 kind2 msg)
    work (Set.intersect inOne inTwo |> List.ofSeq)

/// <summary>
/// Try to unify a top-level type.
/// </summary>
/// <param name="n">Name of the type</param>
/// <param name="kind">ValueType of the type</param>
/// <param name="oldKinds">Previously known types</param>
let tryUnifyMap (n : string) kind oldKinds =
    match Map.tryFind n oldKinds with
    | Some oldKind ->
        match tryUnify(oldKind, kind) with
        | Ok kind ->
            Ok(Map.add n kind oldKinds)
        | Error msg ->
            Error(mkFailedUnification oldKind kind msg)
    | None ->
        Ok(Map.add n kind oldKinds)

let unifyMap n kind oldKinds =
    match tryUnifyMap n kind oldKinds with
    | Ok d -> d
    | Error msg -> raise(UnificationFailure(msg))

/// <summary>
/// Try to unify a possibly multiply occuring field in a composite.
/// </summary>
/// <param name="n">Name of the field</param>
/// <param name="kind">ValueType of the present occurence of the field</param>
/// <param name="minMult">Minimum multiplicity of the field</param>
/// <param name="maxMult">Maximum multiplicity of the field</param>
/// <param name="oldKinds">Map of previously known fields, possibly including the field under consideration and its multiplicity.</param>
let tryUnifyMultMap (n : string) (kind, minMult, maxMult) oldKinds =
    match Map.tryFind n oldKinds with
    | Some (oldKind, oldMin, oldMax) ->
        match tryUnify(oldKind, kind) with
        | Ok kind ->
            Ok(Map.add n (kind, MinOne, Multiple) oldKinds)
        | Error msg ->
            Error(mkFailedUnification oldKind kind msg)
    | None ->
        Ok(Map.add n (kind, MinOne, MaxOne) oldKinds)

let unifyMultMap n kind oldKinds =
    match tryUnifyMultMap n kind oldKinds with
    | Ok d -> d
    | Error msg -> raise(UnificationFailure(msg))
