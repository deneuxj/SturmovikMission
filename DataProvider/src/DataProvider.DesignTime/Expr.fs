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

namespace SturmovikMission.Expr

open System.Collections.Generic

module internal AstExtensions =
    open FSharp.Quotations
    open SturmovikMission.DataProvider.Ast
    open SturmovikMission

    let private valueTypeToExprCache = new Dictionary<ValueType, Expr<ValueType>>(HashIdentity.Structural)

    let buildExprFromValueType expr =
        let mutable idx = 0
        let newVar() =
            idx <- idx + 1
            Var(sprintf "_%d" idx, typeof<ValueType>)
        let getFinalVar : (Var * Expr<ValueType>)[] -> Expr = Array.last >> fst >> Expr.Var
        let rec mkVarsAndDefs (typ : ValueType) =
            [|
                match typ with
                | ValueType.Boolean -> yield newVar(), <@ ValueType.Boolean @>
                | ValueType.Integer -> yield newVar(), <@ ValueType.Integer @>
                | ValueType.String -> yield newVar(), <@ ValueType.String @>
                | ValueType.Float  -> yield newVar(), <@ ValueType.Float @>
                | ValueType.Date -> yield newVar(), <@ ValueType.Date @>
                | ValueType.IntVector -> yield newVar(), <@ ValueType.IntVector @>
                | ValueType.Mapping vt ->
                    let before = mkVarsAndDefs vt
                    let v = getFinalVar before
                    yield! before
                    yield newVar(), <@ ValueType.Mapping %%v @>
                | ValueType.List vt ->
                    let before = mkVarsAndDefs vt
                    let v = getFinalVar before
                    yield! before
                    yield newVar(), <@ ValueType.List %%v @>
                | ValueType.Pair (p1, p2) ->
                    let before1 = mkVarsAndDefs p1
                    let v1 = getFinalVar before1
                    let before2 = mkVarsAndDefs p2
                    let v2 = getFinalVar before2
                    yield! before1
                    yield! before2
                    yield newVar(), <@ ValueType.Pair(%%v1, %%v2) @>
                | ValueType.Triplet (p1, p2, p3) ->
                    let before1 = mkVarsAndDefs p1
                    let v1 = getFinalVar before1
                    let before2 = mkVarsAndDefs p2
                    let v2 = getFinalVar before2
                    let before3 = mkVarsAndDefs p3
                    let v3 = getFinalVar before2
                    yield! before1
                    yield! before2
                    yield! before3
                    yield newVar(), <@ ValueType.Triplet(%%v1, %%v2, %%v3) @>
                | ValueType.FloatPair ->
                    yield newVar(), <@ ValueType.FloatPair @>
                | ValueType.Composite components when components.IsEmpty ->
                    yield newVar(), <@ ValueType.Composite Map.empty @>
                | ValueType.Composite components ->
                    let mutable componentVars = Map.empty
                    let components = Map.toList components
                    for (name, (t, _, _)) in components do
                        let before = mkVarsAndDefs t
                        let v = getFinalVar before
                        componentVars <- componentVars.Add(name, v)
                        yield! before
                    let mapping = Var("mapping", typeof<Map<string, ValueType * MinMultiplicity * MaxMultiplicity>>, isMutable=true)
                    let mappingV =
                        Expr.Var mapping
                        |> Expr.Cast<Map<_, _>>
                    let buildMapping =
                        components
                        |> List.map (fun (name, (_, m, M)) ->
                            let vt = componentVars.[name]
                            let m = m.ToExpr()
                            let M = M.ToExpr()
                            Expr.VarSet(mapping, <@@ Map.add name (%%vt, %m, %M) (%mappingV) @@>)
                            |> Expr.Cast<unit>)
                        |> List.fold (fun e1 e2 -> <@ %e2;%e1 @>) <@ ignore() @>
                        |> fun e ->
                            Expr.Let(mapping, <@@ Map.empty : Map<string, ValueType * MinMultiplicity * MaxMultiplicity> @@>, e)
                    let expr =
                        <@
                            (%%buildMapping : unit)
                            %mappingV
                        @>
                    yield newVar(), <@ ValueType.Composite %expr @>
            |]
        let whole = mkVarsAndDefs expr
        let v = getFinalVar whole
        (whole, v) ||> Array.foldBack (fun (v, defn) e ->
            Expr.Let(v, defn, e)
        )
        |> Expr.Cast<ValueType>

    let getExprOfValueType expr =
        Cached.cached valueTypeToExprCache buildExprFromValueType expr

    type ValueType
        with member this.ToExpr() = getExprOfValueType this

module internal ExprExtensions =
    open FSharp.Quotations
    open ProviderImplementation.ProvidedTypes.UncheckedQuotations
    open ProviderImplementation.ProvidedTypes

    type Expr with
        /// Convert a raw expression of some generarted type to a typed expression with a given target type.
        /// This is useful to insert values with generated types into quotation holes using their base type.
        /// The type of the expression must be assignable to 'TargetType
        static member Convert<'TargetType>(e : Expr) =
            // Should work because it's an upcast
            Expr.Coerce(e, typeof<'TargetType>)
            |> Expr.Cast<'TargetType>

        /// Convert a raw expression representing an IEnumerable<some generated type> to an IEnumerable<'TargetType>
        /// This is useful to insert values which are sequences of some generated type into quotation holes using a sequence of their base type.
        /// The type of items in the sequence in the expression must be assignable to 'TargetType
        static member ConvertEnumerable<'TargetType>(e : Expr) =
            // Might work if cast in the CLR directly supports covariance.
            Expr.Coerce(e, typeof<IEnumerable<'TargetType>>)
            |> Expr.Cast<IEnumerable<'TargetType>>

        /// Convert a raw expression representing a Map<Key, some generated type SourceType> to a Map<Key, TargetType>
        /// This is useful to insert values which are sequences of some generated type into quotation holes using a sequence of their base type.
        /// The type of values in the map in the expression must be assignable to 'TargetType
        static member ConvertMap<'Key, 'TargetType when 'Key : comparison>(e : Expr) =
            // Here we can't rely on covariance. Map<> is an IEnumerable<KeyValuePair<,>>, and KeyValuePair is not covariant.
            // We have no choice but to rebuild the map, after upcasting each value.
            // Accessing the key and value of each KeyValuePair is a PITA because of generics:
            // - Type.MakeGenericType doesn't work with generated types
            // - ProvidedTypeBuilder.MakeGenericType generates calls suffixed with @inst which can't be found in the target assembly
            // The last resort is to use reflection at runtime. It's a bit risky, as we can't catch bugs before they are out in the wild.
            let asEnum = Expr.Convert<System.Collections.IEnumerable>(e)
            <@
                let asEnum = %asEnum
                let kvpTyp = typedefof<KeyValuePair<_, _>>
                let getKey =
                    let propKey = kvpTyp.GetProperty("Key")
                    fun (kvp : obj) -> propKey.GetValue(kvp) :?> 'Key
                let getValue =
                    let propKey = kvpTyp.GetProperty("Value")
                    fun (kvp : obj) -> propKey.GetValue(kvp) :?> 'TargetType
                seq {
                    for kvp in asEnum do
                        yield getKey kvp, getValue kvp
                }
                |> Map.ofSeq
            @> : Expr<Map<'Key, 'TargetType>>

        /// Convert a raw expression representing an option of a generated type to a 'TargetType option
        static member ConvertOpt<'TargetType>(e : Expr) =
            // Same principle used in ConvertMap
            let asObj = Expr.Convert<obj>(e)
            <@
                let asObj = %asObj
                let optTyp = typedefof<_ option>
                let isSome =
                    let propKey = optTyp.GetProperty("IsSome")
                    fun (opt : obj) -> unbox<bool>(propKey.GetValue(opt))
                let getValue =
                    let propKey = optTyp.GetProperty("Value")
                    fun (opt : obj) -> propKey.GetValue(opt) :?> 'TargetType
                if isSome asObj then
                    Some(getValue asObj)
                else
                    None
            @>