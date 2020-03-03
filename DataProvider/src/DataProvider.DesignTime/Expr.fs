﻿//    Copyright 2020 Johann Deneux
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

    type ReflectionAccess with
        static member MiMapMap(tk, t1, t2) =
            ProvidedTypeBuilder.MakeGenericMethod(ReflectionAccess.GetStaticMethod("MapMap"), [tk; t1; t2])

    type Expr with
        /// Convert a raw expression to a typed expression with a given target type.
        /// This is useful to insert values with generated types into quotation holes using their base type.
        static member Convert<'TargetType>(e : Expr) =
            Expr.Coerce(e, typeof<'TargetType>)
            |> Expr.Cast<'TargetType>

        /// Convert a raw expression representing an IEnumerable<> to an IEnumerable<'TargetType>
        /// This is useful to insert values which are sequences of some generated type into quotation holes using a sequence of their base type.
        static member ConvertEnumerable<'TargetType>(e : Expr) =
            Expr.Coerce(e, typeof<IEnumerable<'TargetType>>)
            |> Expr.Cast<IEnumerable<'TargetType>>

        /// Convert a raw expression representing a Map<Key, some generated type SourceType> to a Map<Key, TargetType>
        /// This is useful to insert values which are sequences of some generated type into quotation holes using a sequence of their base type.
        static member ConvertMap<'Key, 'TargetType when 'Key : comparison>(e : Expr) =
            let sourceItemType = e.Type.GetGenericArguments().[1]
            let miMap = ReflectionAccess.MiMapMap(typeof<'Key>, sourceItemType, typeof<'TargetType>)
            Expr.CallUnchecked(
                miMap,
                [
                    let xVar = Quotations.Var("x", sourceItemType) in Expr.Lambda(xVar, Expr.Coerce(Expr.Var xVar, typeof<'TargetType>));
                    e
                ]
            )