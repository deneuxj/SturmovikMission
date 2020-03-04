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


module internal ExprExtensions =
    open FSharp.Quotations
    open System.Collections.Generic
    open ProviderImplementation.ProvidedTypes.UncheckedQuotations
    open ProviderImplementation.ProvidedTypes
    open SturmovikMission.Util

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