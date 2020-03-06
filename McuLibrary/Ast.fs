﻿//    Copyright 2015 Johann Deneux
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


module SturmovikMission.DataProvider.Ast

open System.Collections.Generic
open SturmovikMission.Cached


type MinMultiplicity = Zero | MinOne
with
    member this.ToExpr() =
        match this with
        | Zero -> <@ Zero @>
        | MinOne -> <@ MinOne @>

type MaxMultiplicity = MaxOne | Multiple
with
    member this.ToExpr() =
        match this with
        | MaxOne -> <@ MaxOne @>
        | Multiple -> <@ Multiple @>
            
let least =
    function
    | Zero, _
    | _, Zero -> Zero
    | MinOne, MinOne -> MinOne

let most =
    function
    | Multiple, _
    | _, Multiple -> Multiple
    | MaxOne, MaxOne -> MaxOne

type ValueType =
    | Boolean
    | Integer
    | String
    | Float
    | Composite of Map<string, ValueType * MinMultiplicity * MaxMultiplicity>
    | Mapping of ValueType // { 1 = XXX1; 2 = XXX2 }
    | List of ValueType // { entries }
    | IntVector // [1, 2, 3]
    | Pair of ValueType * ValueType
    | Triplet of ValueType * ValueType * ValueType
    | Date
    | FloatPair

let groundValueTypes =
    [ ValueType.Boolean; ValueType.Date; ValueType.Float; ValueType.FloatPair; ValueType.IntVector; ValueType.Integer; ValueType.String ]

let groundValueTypeNames =
    [ "Boolean"; "Date"; "Float"; "FloatPair"; "IntVector"; "Integer"; "String" ]

let isGroundType x =
    match x with
    | Boolean | Integer | String | Float | IntVector | Date | FloatPair -> true
    | _ -> false

let (|NameOfGroundType|_|) x =
    Seq.zip groundValueTypes groundValueTypeNames
    |> Seq.tryFind(fun (t, n) -> x = t)
    |> Option.map snd

let private valueTypeToExprCache = new Dictionary<ValueType, Quotations.Expr<ValueType>>(HashIdentity.Structural)

let buildExprFromValueType expr =
    let mutable idx = 0
    let newVar() =
        idx <- idx + 1
        Quotations.Var(sprintf "_%d" idx, typeof<ValueType>)
    let getFinalVar : (Quotations.Var * Quotations.Expr<ValueType>)[] -> Quotations.Expr = Array.last >> fst >> Quotations.Expr.Var
    let rec mkVarsAndDefs typ =
        [|
            match typ with
            | Boolean -> yield newVar(), <@ Boolean @>
            | Integer -> yield newVar(), <@ Integer @>
            | String -> yield newVar(), <@ String @>
            | Float  -> yield newVar(), <@ Float @>
            | Date -> yield newVar(), <@ Date @>
            | IntVector -> yield newVar(), <@ IntVector @>
            | Mapping vt ->
                let before = mkVarsAndDefs vt
                let v = getFinalVar before
                yield! before
                yield newVar(), <@ Mapping %%v @>
            | List vt ->
                let before = mkVarsAndDefs vt
                let v = getFinalVar before
                yield! before
                yield newVar(), <@ List %%v @>
            | Pair (p1, p2) ->
                let before1 = mkVarsAndDefs p1
                let v1 = getFinalVar before1
                let before2 = mkVarsAndDefs p2
                let v2 = getFinalVar before2
                yield! before1
                yield! before2
                yield newVar(), <@ Pair(%%v1, %%v2) @>
            | Triplet (p1, p2, p3) ->
                let before1 = mkVarsAndDefs p1
                let v1 = getFinalVar before1
                let before2 = mkVarsAndDefs p2
                let v2 = getFinalVar before2
                let before3 = mkVarsAndDefs p3
                let v3 = getFinalVar before2
                yield! before1
                yield! before2
                yield! before3
                yield newVar(), <@ Triplet(%%v1, %%v2, %%v3) @>
            | FloatPair ->
                yield newVar(), <@ FloatPair @>
            | Composite components when components.IsEmpty ->
                yield newVar(), <@ Composite Map.empty @>
            | Composite components ->
                let mutable componentVars = Map.empty
                let components = Map.toList components
                for (name, (t, _, _)) in components do
                    let before = mkVarsAndDefs t
                    let v = getFinalVar before
                    componentVars <- componentVars.Add(name, v)
                    yield! before
                let mapping = Quotations.Var("mapping", typeof<Map<string, ValueType * MinMultiplicity * MaxMultiplicity>>, isMutable=true)
                let mappingV =
                    Quotations.Expr.Var mapping
                    |> Quotations.Expr.Cast<Map<_, _>>
                let buildMapping =
                    components
                    |> List.map (fun (name, (_, m, M)) ->
                        let vt = componentVars.[name]
                        let m = m.ToExpr()
                        let M = M.ToExpr()
                        Quotations.Expr.VarSet(mapping, <@@ Map.add name (%%vt, %m, %M) (%mappingV) @@>)
                        |> Quotations.Expr.Cast<unit>)
                    |> List.fold (fun e1 e2 -> <@ %e2;%e1 @>) <@ ignore() @>
                    |> fun e ->
                        Quotations.Expr.Let(mapping, <@@ Map.empty : Map<string, ValueType * MinMultiplicity * MaxMultiplicity> @@>, e)
                let expr =
                    <@
                        (%%buildMapping : unit)
                        %mappingV
                    @>
                yield newVar(), <@ Composite %expr @>
        |]
    let whole = mkVarsAndDefs expr
    let v = getFinalVar whole
    (whole, v) ||> Array.foldBack (fun (v, defn) e ->
        Quotations.Expr.Let(v, defn, e)
    )
    |> Quotations.Expr.Cast<ValueType>

let getExprOfValueType expr =
    cached valueTypeToExprCache buildExprFromValueType expr

type ValueType
    with member this.ToExpr() = getExprOfValueType this

type Value =
    | Boolean of bool
    | Integer of int
    | String of string
    | Float of float
    | FloatPair of float * float
    | Composite of (string * Value) list
    | Mapping of (int * Value) list
    | List of Value list
    | IntVector of int list
    | Pair of Value * Value
    | Triplet of Value * Value * Value
    | Date of int * int * int // Day, month, year
with
    member this.GetBool() =
        match this with
        | Boolean b -> b
        | _ -> invalidOp "Not a Boolean"
    member this.GetInteger() =
        match this with
        | Integer n -> n
        | _ -> invalidOp "Not an Integer"
    member this.GetString() =
        match this with
        | String s -> s
        | _ -> invalidOp "Not a String"
    member this.GetFloat() =
        match this with
        | Float x -> x
        | _ -> invalidOp "Not a Float"
    member this.GetFloatPair() =
        match this with
        | FloatPair(x, y) -> x, y
        | _ -> invalidOp "Not a FloatPair"
    member this.GetIntVector() =
        match this with
        | IntVector xs -> xs
        | _ -> invalidOp "Not an IntVector"
    member this.GetDate() =
        match this with
        | Date (d, m, y) -> (d, m, y)
        | _ -> invalidOp "Not a Date"
    member this.GetItems() =
        match this with
        | Composite items -> items
        | _ -> invalidOp "Not a Composite"
    member this.GetMapping() =
        match this with
        | Mapping items -> items
        | _ -> invalidOp "Not a Mapping"
    member this.GetList() =
        match this with
        | List items -> items
        | _ -> invalidOp "Not a Set"
    member this.GetPair() =
        match this with
        | Pair(x1, x2) -> x1, x2
        | _ -> invalidOp "Not a Pair"
    member this.GetTriplet() =
        match this with
        | Triplet(x1, x2, x3) -> x1, x2, x3
        | _ -> invalidOp "Not a Triplet"
    member this.SetItem(name, value) =
        match this with
        | Composite items ->
            let content =
                items
                |> List.filter(fun (name2, _) -> name2 <> name)
            Composite ((name, value) :: content)
        | _ -> invalidOp "Not a Composite"
    member this.SetItem(name, value : Value option) =
        match this with
        | Composite items ->
            let content =
                items
                |> List.filter(fun (name2, _) -> name2 <> name)
            match value with
            | None ->
                Composite content
            | Some value ->
                Composite ((name, value) :: content)
        | _ -> invalidOp "Not a Composite"
    member this.AddItems(name, values : Value list) =
        match this with
        | Composite items ->
            let values =
                values
                |> List.map (fun x -> (name, x))
            Composite (values @ items)
        | _ -> invalidOp "Not a Composite"
    member this.ClearItems(name) =
        match this with
        | Composite items ->
            let content =
                items
                |> List.filter(fun (name2, _) -> name2 <> name)
            Composite content
        | _ -> invalidOp "Not a Composite"
    member this.SetItem(key : int, value) =
        match this with
        | Mapping items ->
            let content =
                items
                |> List.filter(fun (key2, _) -> key2 <> key)
            Mapping ((key, value) :: content)
        | _ -> invalidOp "Not a Mapping"
    member this.RemoveItem(key : int) =
        match this with
        | Mapping items ->
            let content =
                items
                |> List.filter(fun (key2, _) -> key2 <> key)
            Mapping content
        | IntVector items ->
            items
            |> List.filter((<>) key)
            |> IntVector
        | _ -> invalidOp "Not a Mapping or IntVector"
    member this.AddItem(key : int) =
        match this with
        | IntVector items ->
            let content =
                items
                |> List.filter((<>) key)
            IntVector (key :: content)
        | _ -> invalidOp "Not anIntVector"
    member this.Clear() =
        match this with
        | Mapping _ -> Mapping []
        | List _ -> List []
        | IntVector _ -> IntVector []
        | _ -> invalidOp "Not a Mapping, Set or IntVector"

let rec defaultValue (typ : ValueType) =
    match typ with
    | ValueType.Boolean -> Boolean false
    | ValueType.Integer -> Integer 0
    | ValueType.String -> String ""
    | ValueType.Float -> Float 0.0
    | ValueType.Composite m ->
        m
        |> Map.toList
        |> List.choose (fun (name, (typ, minMult, maxMult)) ->
            match minMult, maxMult with
            | MinOne, MaxOne -> Some (name, defaultValue typ)
            | Zero, _ -> None
            | MinOne, Multiple -> Some (name, List [defaultValue typ]))
        |> List.fold (fun e x -> x :: e) []
        |> fun xs -> Composite xs
    | ValueType.Mapping _ -> Mapping []
    | ValueType.List _ -> List []
    | ValueType.IntVector _ -> IntVector []
    | ValueType.Pair (t1, t2) -> Pair(defaultValue t1, defaultValue t2)
    | ValueType.Triplet (t1, t2, t3) -> Triplet(defaultValue t1, defaultValue t2, defaultValue t3)
    | ValueType.FloatPair -> FloatPair(0.0, 0.0)
    | ValueType.Date -> Date(1900, 1, 1)

let rec dump (value : Value) : string =
    match value with
    | Boolean b -> if b then "1" else "0"
    | Integer i -> sprintf "%d" i
    | String s -> sprintf "\"%s\"" s
    | Float f -> sprintf "%f" f
    | FloatPair(x, y) -> sprintf "%f, %f" x y
    | Composite content ->
        let content =
            content
            |> List.sortBy (function
                | _, Mapping _ -> 2
                | _ -> 1
            )
        seq {
            yield sprintf "{\n"
            for (k, v) in content do
                match v with
                | List _
                | Mapping _
                | Composite _ ->
                    yield sprintf "%s\n%s" k (dump v)
                | _ ->
                    yield sprintf "%s = %s;\n" k (dump v)
            yield "}\n"
        }
        |> String.concat ""
    | Date (day, month, year) ->
        sprintf "%d.%d.%d" day month year
    | Pair (x1, x2) ->
        sprintf "%s : %s" (dump x1) (dump x2)
    | Triplet (x1, x2, x3) ->
        sprintf "%s : %s : %s" (dump x1) (dump x2) (dump x3)
    | IntVector xs ->
        let content = 
            seq {
                for x in xs do
                    yield sprintf "%d" x
            }
            |> String.concat ", "
        sprintf "[%s]" content
    | List xs ->
        seq {
            yield "{\n"
            for x in xs do
                yield dump x
                yield ";\n"
            yield "}\n"
        }
        |> String.concat ""
    | Mapping content ->
        seq {
            yield sprintf "{\n"
            for (k, v) in content do
                yield sprintf "%d = %s;\n" k (dump v)
            yield "}\n"
        }
        |> String.concat ""


type Data =
    | Leaf of string * Value
    | Group of GroupData

and GroupData =
    { Name : string
      Index : int
      Description : string
      Data : Data list
    }

type Data with
    member this.GetLeaves() =
        match this with
        | Leaf(name, value) -> [(name, value)]
        | Group group ->
            group.Data
            |> List.map (fun data -> data.GetLeaves())
            |> List.concat

    /// <summary>
    /// Return the leaves with their path (from the leaf to the root).
    /// </summary>
    /// <param name="path">
    /// Path from the group that contains that node to the root.
    /// One item in the path is composed of the name and the index of the corresponding group node.
    /// </param>
    member this.GetLeavesWithPath(path : (string * int) list) =
        match this with
        | Leaf(name, value) -> [(path, name, value)]
        | Group group ->
            group.Data
            |> List.map (fun data -> data.GetLeavesWithPath((group.Name, group.Index) :: path))
            |> List.concat

    member this.GetLeavesWithPath() = this.GetLeavesWithPath([])

    member this.FindByPath(path : string list) =
        match path with
        | [] -> [this]
        | current :: rest ->
            match this with
            | Leaf _ -> []
            | Group group ->
                if group.Name = current then
                    group.Data
                    |> List.map (fun node -> node.FindByPath(rest))
                    |> List.concat
                else
                    []


