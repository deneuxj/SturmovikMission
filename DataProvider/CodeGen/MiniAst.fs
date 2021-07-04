//    Copyright 2020, 2021 Johann Deneux
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

/// Minimal AST and helpers to produce indented F# code.
namespace SturmovikMission.MiniAst

open SturmovikMission
open MBrace.FsPickler
open System.Collections.Generic

/// A type
type Kind =
    Kind of string
with
    member this.WithModule(modul : string) =
        Kind $"{modul}.{this}"

    member this.Option =
        let (Kind s) = this
        Kind $"{s} option"

    member this.Seq =
        let (Kind s) = this
        Kind $"{s} seq"

    override this.ToString() =
        let (Kind s) = this
        s

/// Verbatim blocks of code with indentation.
type MyAst =
    | Leaf of {| Code: string list; Indent: int |}
    | Node of {| Children: MyAst list; Indent: int |}
    | EmptyLine

    member this.Indent(offset) =
        match this with
        | Leaf x -> Leaf {| x with Indent = x.Indent + offset |}
        | Node x -> Node {| x with Indent = x.Indent + offset |}
        | EmptyLine -> EmptyLine

    member this.Lines(?baseIndent : int) =
        let baseIndent = defaultArg baseIndent 0
        seq {
            match this with
            | Leaf x ->
                for line in x.Code do
                    yield sprintf "%s%s\n" (System.String(' ', 4 * (baseIndent + x.Indent))) line
            | Node x ->
                for child in x.Children do
                    yield! child.Lines(baseIndent + x.Indent)
            | EmptyLine ->
                yield "\n"
        }

/// Typed version of MyAst.
type MyAst<'T> = { Untyped : MyAst }

[<RequireQualifiedAccess>]
module MyAst =

    /// A single line of code
    let line s = Leaf {| Code = [s]; Indent = 0 |}

    /// A number of lines of code with the same indentation
    let leaf xs = Leaf {| Code = xs; Indent = 0 |}

    /// Combine MyAst nodes into a node
    let node indent xs = Node {| Children = xs; Indent = indent |}

    /// Check if a MyAst can be rendered as a single line
    let asSingleLine (x : MyAst) =
        match x with
        | Leaf x when x.Indent > 0 -> None
        | Node x when x.Indent > 0 -> None
        | EmptyLine -> None
        | _ ->
        let lines = x.Lines(0)
        if Seq.length lines = 1 then
            Seq.tryHead lines
            |> Option.map (fun s -> s.TrimEnd())
        else
            None

    /// Combine two MyAst with an infix operator
    let infix op left right =
        match asSingleLine left, asSingleLine right with
        | Some left, Some right ->
            line $"{left}{op}{right}"
        | Some left, None ->
            node 0 [
                line $"{left}{op}"
                right.Indent(1)
            ]
        | None, _ ->
            node 0 [
                line "("
                node 0 [
                    line "("
                    left.Indent(1)
                    line ")"
                ]
                line op
                node 0 [
                    line "("
                    right.Indent(1)
                    line ")"
                ]
                line ")"
            ]

    /// Wrap a MyAst in parentheses
    let paren e =
        match asSingleLine e with
        | Some s ->
            line $"({s})"
        | None ->
            node 0 [
                line "("
                e.Indent(1)
                line ")"
            ]

    /// Create the MyAst representing a function call
    let call left right =
        infix " " left (paren right)

    /// Mark a MyAst with a type
    let typed<'T> e : MyAst<'T> = { Untyped = e }

    /// Extract the MyAst from a typed MyAst
    let untyped (e : MyAst<_>) = e.Untyped

    /// Build the MyAst representing the application of a map function to items of type 'T in a sequence.
    let mapSeq (map : MyAst<'T> -> MyAst) (values : MyAst<'T seq>) =
        node 0 [
            yield values.Untyped
            yield "|> Seq.map (fun x ->" |> line
            yield map(line "x" |> typed<'T>).Indent(1)
            yield ")" |> line
        ]

    /// Build the MyAst representing the application of a map function to the values of a mapping.
    let mapValues (map : MyAst<'T> -> MyAst) (values : MyAst<Map<_, 'T>>) =
        node 0 [
            yield values.Untyped
            yield "|> Map.map (fun _ x ->" |> line
            yield map(line "x" |> typed<'T>).Indent(1)
            yield ")" |> line
        ]

    /// Build the MyAst representing the application of a map to an option.
    let mapOption (map : MyAst<'T> -> MyAst) (value : MyAst<'T option>) =
        node 0 [
            yield value.Untyped
            yield "|> Option.map (fun x ->" |> line
            yield map(line "x" |> typed<'T>).Indent(1)
            yield ")" |> line
        ]

[<AutoOpen>]
module ValueTypeExtensions =
    open SturmovikMission.DataProvider
    let private valueTypeToExprCache = new Dictionary<Ast.ValueType, MyAst<Ast.ValueType>>(HashIdentity.Structural)
    let serializer = XmlSerializer()

    /// Serialize a ValueType and return an expression that deserializes that.
    let buildExprFromValueType (typ : SturmovikMission.DataProvider.Ast.ValueType) =
        use writer = new System.IO.StringWriter()
        serializer.Serialize(writer, typ)
        let s = writer.ToString()
        [
            $"let reader = new System.IO.StringReader(\"\"\"{s}\"\"\")"
            "let serializer = XmlSerializer()"
            "serializer.Deserialize<ValueType>(reader)"
        ]
        |> MyAst.leaf
        |> MyAst.typed<Ast.ValueType>

    let getExprOfValueType expr =
        Cached.cached valueTypeToExprCache buildExprFromValueType expr

    type SturmovikMission.DataProvider.Ast.ValueType with
        /// Serialize a ValueType and return an expression that deserializes that.
        member this.ToExpr() = getExprOfValueType this

        /// Serialize a mapping from strings to ValueType, and return an expression that deserializes that.
        static member MapToExpr(mapping : Map<string, Ast.ValueType>) : MyAst<Map<string, Ast.ValueType>> =
            use writer = new System.IO.StringWriter()
            serializer.Serialize(writer, mapping)
            let s = writer.ToString()
            [
                $"let reader = new System.IO.StringReader(\"\"\"{s}\"\"\")"
                "let serializer = XmlSerializer()"
                $"serializer.Deserialize<Map<string, ValueType>>(reader)"
            ]
            |> MyAst.leaf
            |> MyAst.typed<Map<string, Ast.ValueType>>

/// The definition of a method or property
type MemberDefinition =
    {
        Name: string
        Doc: string list
        Kind: Kind
        Args: (string * Kind) list option
        Body: MyAst
        IsStatic: bool
    }

    member this.Ast =
        let argList =
            this.Args
            |> Option.map (
                List.map (fun (argName, Kind kind) -> $"{argName} : {kind}")
                >> String.concat ", "
                >> sprintf "(%s)"
            )
            |> Option.defaultValue ""
        let start =
            if this.IsStatic then
                "static member "
            else
                "member this."
        MyAst.node 0 [
            yield MyAst.EmptyLine
            for line in this.Doc do
                yield $"/// {line}" |> MyAst.line
            yield $"{start}{this.Name}{argList} : {this.Kind} =" |> MyAst.line
            yield this.Body.Indent(1)
        ]

/// The definition of a class
type ClassDefinition =
    {
        Name: string
        Doc: string list
        Args: (string * Kind) list
        Body: MyAst
        Members: ResizeArray<MemberDefinition>
        ParentModuleName : string option
    }
    member this.AddMember(m) =
        this.Members.Add m

    member this.AsKind() =
        Kind(this.Name)

    member this.AsModuleKind() =
        match this.ParentModuleName with
        | None ->
            Kind(this.Name)
        | Some parent ->
            Kind(this.Name).WithModule(parent)

    member this.Ast =
        let args =
            this.Args
            |> List.map (fun (argName, Kind kind) -> $"{argName} : {kind}")
            |> String.concat ", "
        MyAst.node 0 [
            yield EmptyLine
            yield EmptyLine
            for line in this.Doc do
                yield $"/// {line}" |> MyAst.line
            yield $"type {this.Name}({args}) = " |> MyAst.line
            yield this.Body.Indent(1)
            for m in this.Members do
                yield m.Ast.Indent(1)
        ]

/// The definition of a module
and ModuleDefinition =
    {
        Name: string
        Content: ResizeArray<Choice<ClassDefinition, ModuleDefinition>>
    }

    member this.AddClass(c) =
        this.Content.Add(Choice1Of2 c)

    member this.AddModule(m) =
        this.Content.Add(Choice2Of2 m)

    member this.Ast =
        MyAst.node 0 [
            yield EmptyLine
            yield $"module {this.Name} =" |> MyAst.line
            for c in this.Content do
                match c with
                | Choice1Of2 cl->
                    yield cl.Ast.Indent(1)
                | Choice2Of2 m ->
                    if m.Content.Count > 0 then
                        yield m.Ast.Indent(1)
        ]