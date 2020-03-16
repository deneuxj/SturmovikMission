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

        /// Apply a map function to items of type 'T and make a sequence of items of type 'fieldType'
        static member MapItems(fieldType, values : Expr<IEnumerable<'T>>, map : Expr<'T> -> Expr) =
            let enumeratorTyp = typeof<IEnumerator<'T>>
            let miMoveNext = typeof<System.Collections.IEnumerator>.GetMethod("MoveNext")
            let miCurrent = enumeratorTyp.GetProperty("Current")
            let itVar = Var("it", enumeratorTyp)
            let auxListTyp = ProvidedTypeBuilder.MakeGenericType(typedefof<ResizeArray<_>>, [fieldType])
            let resTyp = ProvidedTypeBuilder.MakeGenericType(typedefof<IEnumerable<_>>, [fieldType])
            let miAdd = auxListTyp.GetMethod("Add")
            let miNewAuxList = auxListTyp.GetConstructor([||])
            let auxVar = Var("aux", auxListTyp)
            // let aux = new ResizeArray<T>()
            Expr.LetUnchecked(auxVar, Expr.NewObjectUnchecked(miNewAuxList, []),
                Expr.Sequential(
                    // let it = values.GetEnumerator()
                    Expr.LetUnchecked(itVar, <@ (%values).GetEnumerator() @>,
                        // while
                        Expr.WhileLoop(
                            // it.MoveNext do 
                            Expr.CallUnchecked(Expr.Var itVar, miMoveNext, []),
                            // aux.Add
                            Expr.CallUnchecked(Expr.Var auxVar, miAdd, [
                                // map(it.Current)
                                map(
                                    Expr.PropertyGetUnchecked(
                                        Expr.Var itVar,
                                        miCurrent,
                                        [])
                                    |> Expr.Cast<'T>
                                )
                            ])
                        )
                    ),
                    // return aux :> IEnumerable<fieldType>
                    Expr.Coerce(
                        Expr.Var(auxVar),
                        resTyp)
                )
            )

        /// Apply a map function on the values of a mapping of type 'K, and make a mapping from the same type of keys to values of type 'valueType'
        static member MapMap(valueType, values : Expr<Map<'K, 'V>>, map : Expr<'V> -> Expr) =
            let asSeq = <@ %values |> Map.toSeq @>
            let map (e : Expr<'K * 'V>) =
                let keyVar = Var("k", typeof<'K>)
                let valVar = Var("v", typeof<'V>)
                Expr.LetUnchecked(keyVar, Expr.TupleGetUnchecked(e, 0),
                    Expr.LetUnchecked(valVar, Expr.TupleGetUnchecked(e, 1),
                        Expr.NewTuple [Expr.Var keyVar; map(Expr.Cast<'V>(Expr.Var valVar))]))
            let pairType = ProvidedTypeBuilder.MakeTupleType([typeof<'K>; valueType])
            let mapped = Expr.MapItems(pairType, asSeq, map)
            let mapType = ProvidedTypeBuilder.MakeGenericType(typedefof<Map<_, _>>, [ typeof<'K>; valueType ])
            let constructor =
                mapType.GetConstructors()
                |> Seq.find(fun constructor ->
                    match constructor.GetParameters() with
                    | [| param |] ->
                        let typ = param.ParameterType
                        typ.IsAssignableFrom(mapped.Type)
                    | _ -> false)
            Expr.NewObjectUnchecked(constructor, [mapped])

        /// Map a 'T option to a 'fieldType' option
        static member MapOption(fieldType, value : Expr<'T option>, map : Expr<'T> -> Expr) =
            let inOptTyp = value.Type
            let propIsSome = inOptTyp.GetProperty("IsSome")
            let propValue = inOptTyp.GetProperty("Value")
            let outOptTyp = ProvidedTypeBuilder.MakeGenericType(typedefof<_ option>, [fieldType])
            let propNone = outOptTyp.GetProperty("None")
            let miNewOpt = outOptTyp.GetMethod("Some")
            let inVar = Var("value", inOptTyp)
            // let e = %e in
            Expr.Let(inVar, value,
                // if e.IsSome
                Expr.IfThenElse(
                    Expr.PropertyGetUnchecked(propIsSome, [Expr.Var inVar]),
                    // then Some(map e.Value)
                    Expr.CallUnchecked(
                        // Some
                        miNewOpt,
                        [
                            map(
                                Expr.PropertyGetUnchecked(propValue, [Expr.Var inVar])
                                |> Expr.Cast<'T>
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
