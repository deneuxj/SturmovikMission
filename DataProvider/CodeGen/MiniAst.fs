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

namespace SturmovikMission.MiniAst

open System

type Kind =
    Kind of string
with
    static member OfType<'T>() =
        Kind(typeof<'T>.Name)

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
                    yield sprintf "%s%s\n" (String(' ', 4 * (baseIndent + x.Indent))) line
            | Node x ->
                for child in x.Children do
                    yield! child.Lines(baseIndent + x.Indent)
            | EmptyLine ->
                yield "\n"
        }

type MyAst<'T> = { Untyped : MyAst }

[<RequireQualifiedAccess>]
module MyAst =

    let line s = Leaf {| Code = [s]; Indent = 0 |}

    let leaf xs = Leaf {| Code = xs; Indent = 0 |}

    let node indent xs = Node {| Children = xs; Indent = indent |}

    let asSingleLine (x : MyAst) =
        match x with
        | Leaf x when x.Indent > 0 -> None
        | Node x when x.Indent > 0 -> None
        | EmptyLine -> None
        | _ ->
        let lines = x.Lines(0)
        if Seq.length lines = 1 then
            Seq.tryHead lines
        else
            None

    let infix op left right =
        match asSingleLine left, asSingleLine right with
        | Some left, Some right ->
            line $"{left}{op}{right}"
        | _ ->
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

    let call left right =
        infix " " left right
        |> paren

    let typed<'T> e : MyAst<'T> = { Untyped = e }

    let untyped (e : MyAst<_>) = e.Untyped


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

type ClassDefinition =
    {
        Name: string
        Doc: string list
        Args: (string * Kind) list
        Body: MyAst
        Members: ResizeArray<MemberDefinition>
    }
    member this.AddMember(m) =
        this.Members.Add m

    member this.AsKind = Kind(this.Name)

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

type ModuleDefinition =
    {
        Name: string
        Content: ResizeArray<Choice<ClassDefinition, ModuleDefinition>>
    }

    member this.AddClass(c) =
        this.Content.Add(Choice1Of2 c)

    member this.AddModyle(m) =
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
                    yield m.Ast.Indent(1)
        ]