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
    open MBrace.FsPickler

    let private valueTypeToExprCache = new Dictionary<ValueType, Expr<ValueType>>(HashIdentity.Structural)
    let serializer = XmlSerializer()

    /// Serialize a ValueType and return an expression that deserializes that.
    let buildExprFromValueType (typ : ValueType) =
        use writer = new System.IO.StringWriter()
        serializer.Serialize(writer, typ)
        let s = writer.ToString()
        let s = Expr.Value(s) |> Expr.Cast<string>
        <@
            let reader = new System.IO.StringReader(%s)
            let serializer = XmlSerializer()
            serializer.Deserialize<ValueType>(reader)
        @>

    let getExprOfValueType expr =
        Cached.cached valueTypeToExprCache buildExprFromValueType expr

    type ValueType with
        /// Serialize a ValueType and return an expression that deserializes that.
        member this.ToExpr() = getExprOfValueType this

        /// Serialize a mapping from arbitrary keys to ValueType, and return an expression that deserializes that.
        static member MapToExpr(mapping : Map<'K, ValueType>) : Expr<Map<'K, ValueType>> =
            use writer = new System.IO.StringWriter()
            serializer.Serialize(writer, mapping)
            let s = writer.ToString()
            let s = Expr.Value(s) |> Expr.Cast<string>
            <@
                let reader = new System.IO.StringReader(%s)
                let serializer = XmlSerializer()
                serializer.Deserialize<Map<'K, ValueType>>(reader)
            @>

/// A type whose sole purpose is to mark places in quotation expressions to replace with other expressions
type PlaceHolder<'T>(x : 'T) =
    do()

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
                let it = asEnum.GetEnumerator()
                if not(it.MoveNext()) then
                    Map.empty : Map<'Key, 'TargetType>
                else
                    let kvpTyp = it.Current.GetType()
                    let getKey =
                        let propKey = kvpTyp.GetProperty("Key")
                        fun (kvp : obj) -> propKey.GetValue(kvp) :?> 'Key
                    let getValue =
                        let propVal = kvpTyp.GetProperty("Value")
                        fun (kvp : obj) -> propVal.GetValue(kvp) :?> 'TargetType
                    seq {
                        for kvp in asEnum do
                            yield getKey kvp, getValue kvp
                    }
                    |> Map.ofSeq
            @>

        /// Convert a raw expression representing an option of a generated type to a 'TargetType option
        static member ConvertOpt<'TargetType>(e : Expr) =
            let optTyp = e.Type
            let propIsSome = optTyp.GetProperty("IsSome")
            let propValue = optTyp.GetProperty("Value")
            let propNone = typeof<'TargetType option>.GetProperty("None")
            let eVar = Quotations.Var("e", optTyp)
            // let e = %e in
            Expr.Let(eVar, e,
                // if e.IsSome
                Expr.IfThenElse(
                    Expr.PropertyGetUnchecked(propIsSome, [Expr.Var eVar]),
                    // then Some(e.Value :> 'TargetType)
                    Expr.CallUnchecked(
                        // Some
                        typeof<'TargetType option>.GetMethod("Some"),
                        [
                            // e :> 'TargetType
                            Expr.Coerce(
                                Expr.PropertyGetUnchecked(propValue, [Expr.Var eVar]),
                                typeof<'TargetType>
                            )
                        ]
                    ),
                    // else
                    Expr.PropertyGetUnchecked(
                        // None
                        propNone, []
                    )
                )
            )
            |> Expr.Cast<'TargetType option>

        /// Replace constructor calls to PlaceHolder(x) by a given single-argument constructor call
        member this.ReplacePlaceHolderConstruction(ci : System.Reflection.ConstructorInfo) =
            System.Diagnostics.Debugger.Launch() |> ignore

            let tgtType = ci.DeclaringType
        
            let isPlaceHolderType(typ : System.Type) =
                typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<PlaceHolder<_>>

            let remakeType (typ : System.Type) =
                if typ.IsGenericType then
                    let genTyp = typ.GetGenericTypeDefinition()
                    let genArgs =
                        typ.GetGenericArguments()
                        |> Array.map (fun typ -> if isPlaceHolderType typ then tgtType else typ)
                    ProvidedTypeBuilder.MakeGenericType(genTyp, List.ofArray genArgs)
                else
                    typ

            let remakeMethod (mi : System.Reflection.MethodInfo) =
                if mi.IsGenericMethod then
                    let genDef = mi.GetGenericMethodDefinition()
                    let genArgs =
                        mi.GetGenericArguments()
                        |> Array.map (fun typ -> if isPlaceHolderType typ then tgtType else typ)
                    ProvidedTypeBuilder.MakeGenericMethod(genDef, List.ofArray genArgs)
                else
                    mi

            let rec work(binding, expr, cont) =
                match expr with
                | Patterns.NewObject(op, args) ->
                    workArgs(binding, args, [], fun args ->
                        let typ = op.DeclaringType
                        if isPlaceHolderType typ then
                            Expr.NewObjectUnchecked(ci, args)
                            |> cont
                        else
                            Expr.NewObject(op, args)
                            |> cont)
                | Patterns.Call(instance, mi, args) ->
                    let mi = remakeMethod mi
                    workArgs(binding, args, [], fun args ->
                        match instance with
                        | None -> Expr.CallUnchecked(mi, args)
                        | Some instance -> Expr.CallUnchecked(instance, mi, args)
                        |> cont)
                | ShapeCombinationUnchecked(rebuild, args) ->
                    workArgs(binding, args, [], fun args ->
                        RebuildShapeCombinationUnchecked(rebuild, args)
                        |> cont)
                | ShapeLambdaUnchecked(var, expr) ->
                    let var = Var(var.Name, remakeType var.Type, var.IsMutable)
                    let binding = Map.add var.Name var binding
                    work(binding, expr, fun expr ->
                        Expr.Lambda(var, expr)
                        |> cont)
                | ShapeVarUnchecked var ->
                    let var = binding.[var.Name]
                    Expr.Var var
                    |> cont

            and workArgs(binding, todo, ready, cont) =
                match todo with
                | [] ->
                    List.rev ready |> cont
                | x :: xs ->
                    work (binding, x, fun x -> workArgs(binding, xs, x :: ready, cont))

            let res = work(Map.empty, this, id)
            res