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
    open SturmovikMission.DataProvider.Ast
    open SturmovikMission
    open SturmovikMission.MiniAst
    open MBrace.FsPickler

    let private valueTypeToExprCache = new Dictionary<ValueType, MyAst<ValueType>>(HashIdentity.Structural)
    let serializer = XmlSerializer()

    /// Serialize a ValueType and return an expression that deserializes that.
    let buildExprFromValueType (typ : ValueType) =
        use writer = new System.IO.StringWriter()
        serializer.Serialize(writer, typ)
        let s = writer.ToString()
        [
            $"let reader = new System.IO.StringReader(\"\"\"{s}\"\"\")"
            "let serializer = XmlSerializer()"
            "serializer.Deserialize<ValueType>(reader)"
        ]
        |> MyAst.leaf
        |> MyAst.typed<ValueType>

    let getExprOfValueType expr =
        Cached.cached valueTypeToExprCache buildExprFromValueType expr

    type ValueType with
        /// Serialize a ValueType and return an expression that deserializes that.
        member this.ToExpr() = getExprOfValueType this

        /// Serialize a mapping from strings to ValueType, and return an expression that deserializes that.
        static member MapToExpr(mapping : Map<string, ValueType>) : MyAst<Map<string, ValueType>> =
            use writer = new System.IO.StringWriter()
            serializer.Serialize(writer, mapping)
            let s = writer.ToString()
            {|
                Indent = 0
                Code = [
                    $"let reader = new System.IO.StringReader(\"\"\"{s}\"\"\")"
                    "let serializer = XmlSerializer()"
                    $"serializer.Deserialize<Map<string, ValueType>>(reader)"
                ]
            |}
            |> MyAst.Leaf
            |> MyAst.typed<Map<string, ValueType>>

    type MyAst with
        /// Convert a raw expression of some generated type to a typed expression with a given target type.
        /// This is useful to insert values with generated types into quotation holes using their base type.
        /// The type of the expression must be assignable to 'TargetType
        static member Convert<'TargetType>(e : MyAst) =
            MyAst.node 0 [
                yield MyAst.line "("
                yield e.Indent(1)
                yield MyAst.line $") :> {typeof<'TargetType>.Name}"
            ]
            |> MyAst.typed<'TargetType>

        /// Convert a raw expression representing an IEnumerable<some generated type> to an IEnumerable<'TargetType>
        /// This is useful to insert values which are sequences of some generated type into quotation holes using a sequence of their base type.
        /// The type of items in the sequence in the expression must be assignable to 'TargetType
        static member ConvertEnumerable<'TargetType>(e : MyAst) =
            MyAst.node 0 [
                yield e
                yield $"|> Seq.map (fun x -> upcast<{typeof<'TargetType>.Name}> x)" |> MyAst.line
            ]
            |> MyAst.typed<'TargetType seq>

        /// Convert a raw expression representing a Map<Key, some generated type SourceType> to a Map<Key, TargetType>
        /// This is useful to insert values which are sequences of some generated type into quotation holes using a sequence of their base type.
        /// The type of values in the map in the expression must be assignable to 'TargetType
        static member ConvertMap<'K, 'TargetType when 'K: comparison>(e : MyAst) =
            MyAst.node 0 [
                yield e
                yield $"|> Map.map (fun k v -> upcast<{typeof<'TargetType>.Name}> v)" |> MyAst.line
            ]
            |> MyAst.typed<Map<'K, 'TargetType>>

        /// Convert a raw expression representing an option of a generated type to a 'TargetType option
        static member ConvertOpt<'TargetType>(e : MyAst) =
            MyAst.node 0 [
                yield e
                yield $"|> Option.map (fun x -> upcast<{typeof<'TargetType>.Name}> v)" |> MyAst.line
            ]
            |> MyAst.typed<'TargetType option>

        /// Apply a map function to items of type 'T and make a sequence of items of type 'fieldType'
        static member MapItems (map : MyAst<'T> -> MyAst) (values : MyAst<'T seq>) =
            MyAst.node 0 [
                yield values.Untyped
                yield "|> Seq.map (fun x ->" |> MyAst.line
                yield map(MyAst.line "x" |> MyAst.typed<'T>).Indent(1)
                yield ")" |> MyAst.line
            ]

        /// Apply a map function on the values of a mapping of type 'K, and make a mapping from the same type of keys to values of type 'valueType'
        static member MapMap (map : MyAst<'T> -> MyAst) (values : MyAst<Map<_, 'T>>) =
            MyAst.node 0 [
                yield values.Untyped
                yield "|> Map.map (fun _ x ->" |> MyAst.line
                yield map(MyAst.line "x" |> MyAst.typed<'T>).Indent(1)
                yield ")" |> MyAst.line
            ]

        /// Map a 'T option to a 'fieldType' option
        static member MapOption (map : MyAst<'T> -> MyAst) (value : MyAst<'T option>) =
            MyAst.node 0 [
                yield value.Untyped
                yield "|> Option.map (fun x ->" |> MyAst.line
                yield map(MyAst.line "x" |> MyAst.typed<'T>).Indent(1)
                yield ")" |> MyAst.line
            ]
