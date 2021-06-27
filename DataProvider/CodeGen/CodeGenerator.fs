//    Copyright 2015, 2020, 2021 Johann Deneux
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


module SturmovikMission.DataProvider.CodeGenerator

open System.Collections.Generic
open System
open System.IO
open SturmovikMission.Cached
open SturmovikMission.DataProvider
open SturmovikMission.MiniAst

module internal Internal =
    open SturmovikMission.Expr.AstExtensions

    [<AutoOpen>]
    module AstValueWrapperTypeBuildingHelpers =
        /// Create a type inheriting from AstValueWrapper, with a constructor taking an Ast.Value
        let newWrapper(name, moduleName) =
            let ptyp : ClassDefinition =
                {
                    Name = name
                    Doc = []
                    Args = ["value", Kind "Ast.Value"]
                    Body = MyAst.line "inherit AstValueWrapper(value)"
                    Members = ResizeArray()
                    ParentModuleName = moduleName
                }
            ptyp

        let newModule(name) : ModuleDefinition =
            {
                Name = name
                Content = ResizeArray()
            }

        let newProperty(name, typ, body : MyAst) =
            let prop : MemberDefinition =
                {
                    Name = name
                    Doc = []
                    Kind = typ
                    Args = None
                    Body = body
                    IsStatic = false
                }
            prop

        let newMethod(name, typ, args, body : MyAst) =
            let meth : MemberDefinition =
                {
                    Name = name
                    Doc = []
                    Kind = typ
                    Args = Some args
                    Body = body
                    IsStatic = false
                }
            meth

        let newStaticProperty(name, typ, body) = { newProperty(name, typ, body) with IsStatic = true }

        let newStaticMethod(name, typ, args, body) = { newMethod(name, typ, args, body) with IsStatic = true }

        /// Create a named constructor, i.e. a public static method to create a new instance
        let addNamedConstructor(name, ptyp : ClassDefinition, args, expr : MyAst) =
            let body =
                [
                    MyAst.line "let value ="
                    expr.Indent(1)
                    MyAst.line <| $"{ptyp.Name}(value)"
                ]
                |> MyAst.node 0
            match args with
            | [] -> ptyp.AddMember(newStaticProperty(name, ptyp.AsKind(), body))
            | _ :: _ -> ptyp.AddMember(newStaticMethod(name, ptyp.AsKind(), args, body))

    /// Type of keys using in caching provided type definitions.
    type TypeIdentification = {
        Name : string
        Kind : Ast.ValueType
        Parents : string list
    }

    let (|GroundType|ComplexType|) kind =
        if Ast.isGroundType kind then GroundType else ComplexType

    /// Get the path to a type in the same module as the current class, other than the class itself
    let typePath(typeId : TypeIdentification, classDef : ClassDefinition) =
        match typeId.Kind with
        | GroundType ->
            classDef.Name
        | ComplexType ->
            match typeId.Parents with
            | parent :: _ -> $"{parent}.{classDef.Name}"
            | _ -> classDef.Name

    /// Create a function that wraps an Ast.Value AST in a constructor call
    let wrap (typeId : TypeIdentification, classDef : ClassDefinition) (e : MyAst<Ast.Value>) =
        let path = typePath(typeId, classDef)
        MyAst.call (MyAst.line path) e.Untyped

    /// Create a function that wraps an Ast.Value AST representing the value of the current instance in the constructor of the current class.
    let build (classDef : ClassDefinition) (e : MyAst<Ast.Value>) =
        MyAst.call (MyAst.line classDef.Name) e.Untyped

    /// Build the function that builds ProvidedTypeDefinitions for ValueTypes encountered in the sample mission file.
    let mkProvidedTypeBuilder logInfo =
        logInfo "Started inferring all MCU value types"

        let cache = new Dictionary<TypeIdentification, ClassDefinition * ModuleDefinition option>(HashIdentity.Structural)

        let asList this = MyAst.line $"({this}.Wrapped : Ast.Value).GetItems()"

        /// Add static property AstType to a generated type. It returns the ValueType.
        let addAstValueTypeProperty (ptyp : ClassDefinition, vt : Ast.ValueType) =
            ptyp.AddMember(newStaticProperty("AstType", Kind "Ast.ValueType", vt.ToExpr().Untyped))

        // Builders for the ground types

        let ptypBoolean =
            let ptyp = newWrapper("Boolean", None)
            ptyp.AddMember(newProperty("Value", Kind "bool", MyAst.line "this.Wrapped.GetBool()"))
            addNamedConstructor("N", ptyp, [("value", Kind "bool")], MyAst.line "Ast.Value.Boolean(value)")
            addNamedConstructor("Default", ptyp, [], MyAst.line "Ast.Value.Boolean false")
            addAstValueTypeProperty(ptyp, Ast.ValueType.Boolean)
            ptyp

        let ptypFloat =
            let ptyp = newWrapper("Float", None)
            ptyp.AddMember(newProperty("Value", Kind "float", MyAst.line "this.Wrapped.GetFloat()"))
            addNamedConstructor("N", ptyp, [("value", Kind "float")], MyAst.line "Ast.Value.Float(value)")
            addNamedConstructor("Default", ptyp, [], MyAst.line "Ast.Value.Float 0.0")
            addAstValueTypeProperty(ptyp, Ast.ValueType.Float)
            ptyp

        let ptypFloatPair =
            let ptyp = newWrapper("FloatPair", None)
            ptyp.AddMember(newProperty("Value", Kind "float * float", MyAst.line "this.Wrapped.GetFloatPair()"))
            addNamedConstructor("N", ptyp, [("value", Kind "float * float")], MyAst.line "Ast.Value.FloatPair(value)")
            addNamedConstructor("Default", ptyp, [], MyAst.line "Ast.Value.FloatPair(0.0, 0.0)")
            addAstValueTypeProperty(ptyp, Ast.ValueType.FloatPair)
            ptyp

        let ptypInteger =
            let ptyp = newWrapper("Integer", None)
            ptyp.AddMember(newProperty("Value", Kind "int", MyAst.line "this.Wrapped.GetInteger()"))
            addNamedConstructor("N", ptyp, [("value", Kind "int")], MyAst.line "Ast.Value.Integer value")
            addNamedConstructor("Default", ptyp, [], MyAst.line "Ast.Value.Integer 0")
            addAstValueTypeProperty(ptyp, Ast.ValueType.Integer)
            ptyp

        let ptypMask =
            let ptyp = newWrapper("Mask", None)
            ptyp.AddMember(newProperty("Value", Kind "int64", MyAst.line "this.Wrapped.GetMask()"))
            addNamedConstructor("N", ptyp, [("value", Kind "int64")], MyAst.line "Ast.Value.Mask value")
            addNamedConstructor("Default", ptyp, [], MyAst.line "Ast.Value.Mask 0L")
            addAstValueTypeProperty(ptyp, Ast.ValueType.Mask)
            ptyp

        let ptypString =
            let ptyp = newWrapper("String", None)
            ptyp.AddMember(newProperty("Value", Kind "string", MyAst.line "this.Wrapped.GetString()"))
            addNamedConstructor("N", ptyp, [("value", Kind "string")], MyAst.line "Ast.Value.String value")
            addNamedConstructor("Default", ptyp, [], MyAst.line "Ast.Value.String \"\"")
            addAstValueTypeProperty(ptyp, Ast.ValueType.String)
            ptyp

        let ptypIntVector =
            let ptyp = newWrapper("VectorOfIntegers", None)
            ptyp.AddMember(newProperty("Value", Kind "int list", MyAst.line "this.Wrapped.GetIntVector()"))
            addNamedConstructor("N", ptyp, [("value", Kind "int list")], MyAst.line "Ast.Value.IntVector value")
            addNamedConstructor("Default", ptyp, [], MyAst.line "Ast.Value.IntVector []")
            addAstValueTypeProperty(ptyp, Ast.ValueType.IntVector)
            ptyp

        let ptypDate =
            let ptyp = newWrapper("Date", None)
            let body =
                MyAst.node 0 [
                    MyAst.line "let _, _, year = this.Wrapped.GetDate()"
                    MyAst.line "year"
                ]
            ptyp.AddMember(newProperty("Year", Kind "int", body))
            let body =
                MyAst.node 0 [
                    MyAst.line "let _, month, _ = this.Wrapped.GetDate()"
                    MyAst.line "month"
                ]
            ptyp.AddMember(newProperty("Month", Kind "int", body))
            let body =
                MyAst.node 0 [
                    MyAst.line "let day, _, _ = this.Wrapped.GetDate()"
                    MyAst.line "day"
                ]
            ptyp.AddMember(newProperty("Day", Kind "int", body))
            addNamedConstructor(
                    "FromDate",
                    ptyp,
                    [("day", Kind "int"); ("month", Kind "int"); ("year", Kind "int")],
                    MyAst.line "Ast.Value.Date(day, month, year)")
            addAstValueTypeProperty(ptyp, Ast.ValueType.Date)
            ptyp

        let addComplexNestedType(ptyp : ModuleDefinition, (subpTyp, submodul) : ClassDefinition * ModuleDefinition option, kind) =
            match kind with
            | GroundType ->
                ()
            | ComplexType ->
                match submodul with
                | Some m ->
                    if ptyp.Content |> Seq.exists (function Choice2Of2 x -> x.Name =  m.Name | _ -> false) |> not then
                        match submodul with
                        | Some m -> ptyp.AddModule(m)
                        | None -> ()
                    else
                        failwithf "Cannot add module '%s' in '%s', there is already a module by that name." m.Name ptyp.Name
                | None ->
                    ()
                if ptyp.Content |> Seq.exists (function Choice1Of2 x -> x.Name =  subpTyp.Name | _ -> false) |> not then
                    ptyp.AddClass(subpTyp)
                else
                    failwithf "Cannot add provided type for '%s' in '%s', there is already a member by that name." subpTyp.Name ptyp.Name

        // Build any kind of type, ground or complex.
        let rec buildProvidedType (typId : TypeIdentification) =
            logInfo <| sprintf "Started building provided type for %s" typId.Name
            try
                let name = typId.Name
                let ptyp, modul =
                    match typId.Kind with
                    | Ast.ValueType.Boolean -> ptypBoolean, None
                    | Ast.ValueType.Float -> ptypFloat, None
                    | Ast.ValueType.FloatPair -> ptypFloatPair, None
                    | Ast.ValueType.Integer -> ptypInteger, None
                    | Ast.ValueType.Mask -> ptypMask, None
                    | Ast.ValueType.String -> ptypString, None
                    | Ast.ValueType.IntVector -> ptypIntVector, None
                    | Ast.ValueType.Date -> ptypDate, None
                    | Ast.ValueType.Pair (typ1, typ2) -> buildPair(typId, typ1, typ2)
                    | Ast.ValueType.Triplet (typ1, typ2, typ3) -> buildTriple(typId, typ1, typ2, typ3)
                    | Ast.ValueType.Composite fields ->
                        let ptyp = newWrapper(name, typId.Parents |> List.tryHead)
                        let modul = newModule(name)
                        let parents = name :: typId.Parents
                        // Add types of complex fields as nested types
                        for field in fields do
                            let fieldName = field.Key
                            let fieldKind, _, _ = field.Value
                            let subpTyp, subModule = getProvidedType { Name = fieldName; Kind = fieldKind; Parents = parents }
                            addComplexNestedType(modul, (subpTyp, subModule), fieldKind)
                        // Getters
                        ptyp.Members.AddRange(getters parents fields)
                        // Setters
                        ptyp.Members.AddRange(setters parents (fields, ptyp))
                        // Create as MCU
                        ptyp.Members.AddRange(asMcu (name, typId.Kind))
                        // Parse
                        ptyp.AddMember(staticParser (typId.Kind, name, ptyp))
                        // Dump to text
                        let meth = newMethod("AsString", Kind "string", [], MyAst.line $"\"{name}\" + (Ast.dump this.Wrapped)")
                        ptyp.AddMember(meth)
                        // Result
                        ptyp, Some modul
                    | Ast.ValueType.Mapping itemTyp ->
                        let subName = sprintf "%s_ValueType" name
                        let ptyp1Id = { Name = subName; Kind = itemTyp; Parents = name :: typId.Parents }
                        let ptyp1, module1 = getProvidedType ptyp1Id
                        let ptyp = newWrapper(name, typId.Parents |> List.tryHead)
                        let modul = newModule(name)
                        addComplexNestedType(modul, (ptyp1, module1), itemTyp)
                        // Constructor from map
                        addNamedConstructor("FromMap", ptyp, [("map", Kind $"Map<int, {ptyp1.Name}>")],
                            MyAst.node 0 [
                                MyAst.line "map"
                                "|> Map.map (fun _ v -> v.Wrapped)" |> MyAst.line
                                "|> Map.toList" |> MyAst.line
                                "|> Ast.Value.Mapping" |> MyAst.line
                            ])
                        // Value getter
                        let propTyp = Kind $"Map<int, {ptyp1.Name}>"
                        let body =
                            let values =
                                MyAst.line "Map.ofList(this.Wrapped.GetMapping())"
                                |> MyAst.typed<Map<int, Ast.Value>>
                            let wrap = wrap (ptyp1Id, ptyp1)
                            MyAst.MapMap wrap values
                        ptyp.AddMember(newProperty("Value", propTyp, body))
                        // Set item in the map
                        let body =
                            "this.Wrapped.SetItem(key, value.Wrapped)"
                            |> MyAst.line
                            |> MyAst.typed<Ast.Value>
                            |> build ptyp
                        ptyp.AddMember(newMethod("SetItem", ptyp.AsKind(), [("key", Kind "int"); ("value", ptyp1.AsModuleKind())], body))
                        // Remove item from the map
                        let body =
                            "this.Wrapped.RemoveItem(key)"
                            |> MyAst.line
                            |> MyAst.typed<Ast.Value>
                            |> build ptyp
                        ptyp.AddMember(newMethod("RemoveItem", ptyp.AsKind(), ["key", Kind "int"], body))
                        // Clear map
                        let body =
                            "Ast.Value.Mapping []"
                            |> MyAst.line
                            |> MyAst.typed<Ast.Value>
                            |> build ptyp
                        ptyp.AddMember(newMethod("Clear", ptyp.AsKind(), [], body))
                        // Result
                        ptyp, Some modul
                    | Ast.ValueType.List itemTyp ->
                        let subName = sprintf "%s_ValueType" name
                        let ptyp1Id = { Name = subName; Kind = itemTyp; Parents = name :: typId.Parents }
                        let ptyp1, module1 = getProvidedType ptyp1Id
                        let ptyp = newWrapper(name, typId.Parents |> List.tryHead)
                        let modul = newModule(name)
                        addComplexNestedType(modul, (ptyp1, module1), itemTyp)
                        // Value getter
                        let propTyp = ptyp1.AsModuleKind().Seq
                        let body =
                            "this.Wrapped.GetList()"
                            |> MyAst.line
                            |> MyAst.typed<Ast.Value seq>
                            |> MyAst.MapItems (wrap (ptyp1Id, ptyp1))
                        ptyp.AddMember(newProperty("Value", propTyp, body))
                        // constructor with value
                        let body =
                            "items |> Seq.map (fun item -> item.Wrapped) |> List.ofSeq |> Ast.Value.List"
                            |> MyAst.line
                        addNamedConstructor(
                            "FromList",
                            ptyp,
                            ["items", propTyp],
                            body
                        )
                        // Result
                        ptyp, Some modul
                // Add a default constructor and the value type property, unless it's a ground type (it's already got those)
                if not (Ast.isGroundType typId.Kind) then
                    let kind =
                        typId.Kind.ToExpr()
                        |> MyAst.untyped
                        |> MyAst.call (MyAst.line "Ast.defaultValue")
                    addNamedConstructor("Default", ptyp, [], kind)
                    addAstValueTypeProperty(ptyp, typId.Kind)
                ptyp, modul
            finally
                logInfo <| sprintf "Done building provided type for %s" typId.Name

        and buildPair (typId : TypeIdentification, typ1 : Ast.ValueType, typ2 : Ast.ValueType) =
            let ptyp1, m1 =
                let subName = sprintf "%s_ValueType1" typId.Name
                getProvidedType { Name = subName; Kind = typ1; Parents = typId.Name :: typId.Parents }
            let ptyp2, m2 =
                let subName = sprintf "%s_ValueType2" typId.Name
                getProvidedType { Name = subName; Kind = typ2; Parents = typId.Name :: typId.Parents }
            let ptyp = newWrapper(typId.Name, typId.Parents |> List.tryHead)
            let modul = newModule(typId.Name)
            addComplexNestedType(modul, (ptyp1, m1), typ1)
            addComplexNestedType(modul, (ptyp2, m2), typ2)
            // Value getter
            let propTyp = Kind $"({ptyp1.Name} * {ptyp2.Name})"
            let body =
                MyAst.node 0 [
                    "let x, y = this.Wrapped.GetPair()" |> MyAst.line
                    $"({ptyp1.Name}(x), {ptyp2.Name}(y))" |> MyAst.line
                ]
            ptyp.AddMember(newProperty("Value", propTyp, body))
            // Constructor
            let body =
                "Ast.Value.Pair(x.Wrapped, y.Wrapped)"
                |> MyAst.line
            addNamedConstructor("Create", ptyp, [("x", ptyp1.AsModuleKind()); ("y", ptyp2.AsModuleKind())], body)
            // Result
            ptyp, Some modul

        and buildTriple (typId : TypeIdentification, typ1 : Ast.ValueType, typ2 : Ast.ValueType, typ3 : Ast.ValueType) =
            let name = typId.Name
            let ptyp1, m1 =
                let subName = sprintf "%s_ValueType1" name
                getProvidedType { Name = subName; Kind = typ1; Parents = name :: typId.Parents }
            let ptyp2, m2 =
                let subName = sprintf "%s_ValueType2" name
                getProvidedType { Name = subName; Kind = typ2; Parents = name :: typId.Parents }
            let ptyp3, m3 =
                let subName = sprintf "%s_ValueType3" name
                getProvidedType { Name = subName; Kind = typ3; Parents = name :: typId.Parents }
            let ptyp = newWrapper(name, typId.Parents |> List.tryHead)
            let modul = newModule(name)
            addComplexNestedType(modul, (ptyp1, m1), typ1)
            addComplexNestedType(modul, (ptyp2, m2), typ2)
            addComplexNestedType(modul, (ptyp3, m3), typ3)
            let propTyp = Kind $"({ptyp1.Name} * {ptyp2.Name} * {ptyp3.Name})"
            // Value getter
            let body =
                MyAst.node 0 [
                    "let x, y, z = this.Wrapped.GetTriplet()" |> MyAst.line
                    $"({ptyp1.Name}(x), {ptyp2.Name}(y), {ptyp3.Name}(z))" |> MyAst.line
                ]
            ptyp.AddMember(newProperty("Value", propTyp, body))
            // Constructor
            let body =
                "Ast.Value.Triplet(x.Wrapped, y.Wrapped, z.Wrapped)"
                |> MyAst.line
            addNamedConstructor("Create", ptyp, [("x", ptyp1.AsModuleKind()); ("y", ptyp2.AsModuleKind()); ("z", ptyp3.AsModuleKind())], body)
            // Result
            ptyp, Some modul

        // Build the getters in composite types
        and getters parents fields =
            fields
            |> Map.map (
                fun fieldName (def, minMult, maxMult) ->
                    let fieldTypeId = { Name = fieldName; Kind = def; Parents = parents }
                    let fieldType, fieldModule =
                        getProvidedType fieldTypeId

                    let wrap = wrap (fieldTypeId, fieldType)

                    match (minMult, maxMult) with
                    | Ast.MinMultiplicity.MinOne, Ast.MaxMultiplicity.MaxOne ->
                        let body =
                            let e = asList "this"
                            MyAst.node 0 [
                                "let e =" |> MyAst.line
                                e.Indent(1)
                                $"match List.tryFind (fun (name, _) -> name = \"{fieldName}\") e with" |> MyAst.line
                                "| Some (_, value) -> value" |> MyAst.line
                                $"| None -> failwithf \"Field '{fieldName}' is not set\"" |> MyAst.line
                            ]
                            |> MyAst.typed<Ast.Value>
                            |> wrap
                        newMethod(sprintf "Get%s" fieldName, fieldType.AsModuleKind(), [], body)
                    | Ast.MinMultiplicity.Zero, Ast.MaxOne ->
                        let optTyp = fieldType.AsModuleKind().Option
                        let body =
                            let e = asList "this"
                            MyAst.node 0 [
                                "let e =" |> MyAst.line
                                e.Indent(1)
                                $"List.tryPick (fun (name, x) -> if name = \"{fieldName}\" then Some x else None) e" |> MyAst.line
                            ]
                            |> MyAst.typed<Ast.Value option>
                            |> MyAst.MapOption wrap
                        newMethod(sprintf "TryGet%s" fieldName, optTyp, [], body)
                    | _, Ast.MaxMultiplicity.Multiple ->
                        let seqTyp = fieldType.AsModuleKind().Seq
                        let body =
                            let fields = asList "this"
                            MyAst.node 0 [
                                "let fields =" |> MyAst.line
                                fields.Indent(1)
                                "fields" |> MyAst.line
                                $"|> List.choose (fun (name, x) -> if name = \"{fieldName}\" then Some x else None)" |> MyAst.line
                            ]
                            |> MyAst.typed<Ast.Value seq>
                            |> MyAst.MapItems wrap
                        newMethod(sprintf "Get%ss" fieldName, seqTyp, [], body)
                )
            |> Map.toList
            |> List.sortBy fst
            |> List.map snd

        // setters in composites, using fluent interfaces
        and setters parents (fields, ptyp) =
            fields
            |> Seq.map(fun kvp ->
                let fieldName = kvp.Key
                let (def, minMult, maxMult) = kvp.Value
                let fieldType, fieldModule =
                    getProvidedType { Name = fieldName; Kind = def; Parents = parents }
                match (minMult, maxMult) with
                | Ast.MinMultiplicity.MinOne, Ast.MaxMultiplicity.MaxOne ->
                    let body =
                        $"{ptyp.Name}(this.Wrapped.SetItem(\"{fieldName}\", value.Wrapped))" |> MyAst.line

                    newMethod(sprintf "Set%s" fieldName, ptyp.AsKind(), [("value", fieldType.AsModuleKind())], body)
                | Ast.MinMultiplicity.Zero, Ast.MaxOne ->
                    let optTyp = fieldType.AsModuleKind().Option
                    let body =
                        MyAst.node 0 [
                            "let arg = value |> Option.map (fun x -> x.Wrapped)" |> MyAst.line
                            $"{ptyp.Name}(this.Wrapped.SetItem(\"{fieldName}\", arg))" |> MyAst.line
                        ]
                    newMethod(sprintf "Set%s" fieldName, ptyp.AsKind(), [("value", optTyp)], body)
                | _, Ast.MaxMultiplicity.Multiple ->
                    let seqTyp = fieldType.AsModuleKind().Seq
                    let body =
                        MyAst.node 0 [
                            $"let xs = value |> Seq.map (fun x -> x.Wrapped)" |> MyAst.line
                            $"let res = this.Wrapped.ClearItems(\"{fieldName}\").AddItems(\"{fieldName}\", List.ofSeq xs)" |> MyAst.line
                            $"{ptyp.Name}(res)" |> MyAst.line
                        ]
                    newMethod(sprintf "Set%s" fieldName, ptyp.AsKind(), [("value", seqTyp)], body)
                )
            |> List.ofSeq

        // Methods to build mutable MCU instances
        and asMcu (name, typ) =
            [
                match McuFactory.tryMakeMcu(name, typ) with
                | Some f ->
                    use writer = new System.IO.StringWriter()
                    serializer.Serialize(writer, fun vt -> f(vt, []))
                    let s = writer.ToString()
                    let body =
                        [
                            $"let reader = new System.IO.StringReader(\"\"\"{s}\"\"\")"
                            "let serializer = XmlSerializer()"
                            "let f = serializer.Deserialize<Ast.Value -> Mcu.McuBase>(reader)"
                            "f(this.Wrapped)"
                        ]
                        |> MyAst.leaf
                    yield newMethod("CreateMcu", Kind "Mcu.McuBase", [], body)
                | None -> ()
            ]

        // static method to create a parser
        and staticParser (valueType : Ast.ValueType, name, ptyp) =
            let vtExpr = valueType.ToExpr()
            let retType = Kind "Parsing.ParserFun"

            let body =
                MyAst.node 0 [
                    MyAst.line "let bodyParser ="
                    (MyAst.call (MyAst.line "Parsing.makeParser") vtExpr.Untyped).Indent(1)
                    MyAst.line "fun (s : Parsing.Stream) ->"
                    MyAst.node 1 [
                        MyAst.line "let (Parsing.SubString(data, offset)) = s"
                        MyAst.line $"if data.Substring(offset).StartsWith(\"{name}\") then"
                        MyAst.node 1 [
                            MyAst.line $"let s = Parsing.SubString(data, offset + {name.Length})"
                            MyAst.line "bodyParser.Run s"
                        ]
                        MyAst.line "else"
                        (MyAst.line $"Parsing.parseError(\"Expected '{name}'\", s)").Indent(1)
                    ]
                    MyAst.line "|> Parsing.ParserFun"
                ]

            newStaticMethod("GetParser", retType, [], body)

        and getProvidedType typId : ClassDefinition * ModuleDefinition option=
            cached cache buildProvidedType typId

        logInfo "Done inferring all MCU value types"

        getProvidedType, cache

    /// <summary>
    /// Build the provided method that builds a list of objects implementing McuBase and its subtypes.
    /// </summary>
    /// <param name="logInfo">Logging function</param>
    /// <param name="namedValueTypes">List of ValueTypes with their name and their provided type definition.</param>
    let buildAsMcuList logInfo (namedValueTypes : (string * Ast.ValueType * ClassDefinition) list) =
        logInfo "Started building the MCU list builder method"
        let valueTypeOfName =
            namedValueTypes
            |> Seq.map (fun (name, vt, _) -> name, vt)
            |> Map.ofSeq
            |> Ast.ValueType.MapToExpr
        let body =
            MyAst.node 0 [
                MyAst.line "let valueTypeOfName ="
                valueTypeOfName.Untyped.Indent(1)
                MyAst.line "let mcuMakerOfName ="
                MyAst.node 1 [
                    MyAst.line "valueTypeOfName"
                    MyAst.line "|> Map.map (fun name vt -> McuFactory.tryMakeMcu(name, vt))"
                ]
                MyAst.line "this.Items"
                MyAst.line "|> List.collect (fun data -> data.GetLeavesWithPath())"
                MyAst.line "|> List.choose (fun (path, name, value) ->"
                MyAst.node 1 [
                    MyAst.line "match Map.tryFind name mcuMakerOfName with"
                    MyAst.line "| Some(Some(make)) -> Some(make(value, path))"
                    MyAst.line "| _ -> None"
                ]
                MyAst.line ")"
            ]

        let method = newMethod("CreateMcuList", Kind "Mcu.McuBase list", [], body)

        logInfo "Done building the MCU list builder method"

        method

    /// <summary>
    /// Build the provided type definition of the type that offers parsing of mission files.
    /// </summary>
    /// <param name="logInfo">Logging function.</param>
    /// <param name="namedValueTypes">ValueTypes with their name and provided type definition.</param>
    /// <param name="topComplexTypes">Complex types that aren't nested in other types.</param>
    let buildGroupParserType logInfo (namedValueTypes : (string * Ast.ValueType * ClassDefinition) list) (topComplexTypes : (string * Ast.ValueType * ClassDefinition) list) =
        logInfo "Started building the group parser type"
        let dataListType = Kind "Ast.Data list"
        let body =
            MyAst.node 0 [
                "inherit GroupMembers(nodes)" |> MyAst.line
            ]
        let parser : ClassDefinition =
            {
                Name = "GroupData"
                Doc = ["Extraction of data from a mission or group file."]
                Args = [("nodes", dataListType)]
                Body = body
                Members = ResizeArray()
                ParentModuleName = None
            }
        let valueTypeOfName =
            namedValueTypes
            |> Seq.map (fun (name, vt, _) -> name, vt)
            |> Map.ofSeq
            |> Ast.ValueType.MapToExpr
        // Static method: Parse a group or mission file
        parser.AddMember(
            let constructor =
                let body =
                    let nodes =
                        MyAst.node 0 [
                            MyAst.line "let parsers ="
                            MyAst.node 1 [
                                valueTypeOfName.Untyped
                                "|> Map.map (fun name valueType -> Parsing.makeParser valueType)" |> MyAst.line
                            ]
                            MyAst.line "let getParser name = parsers.[name]"
                            MyAst.line "Parsing.parseFile getParser s"
                        ]
                    MyAst.call (MyAst.line "GroupData") nodes
                newStaticMethod("Parse", parser.AsKind(), [("s", Kind "Parsing.Stream")], body)
            { constructor with
                Doc = [
                    """<summary>Parse a mission or group file and store the extracted data.</summary>"""
                    """<param name="s">The stream that is parsed</param>"""
                    """<exception cref="Parsing.ParseError">Failed to parse the mission or group</exception>"""
                ]
            })
        // Get data from a subgroup
        parser.AddMember(
            let constructor =
                let body =
                    let nodes =
                        MyAst.node 0 [
                            MyAst.line "this.Items"
                            MyAst.line "|> List.collect (fun node -> node.FindByPath [name])"
                        ]
                    MyAst.call (MyAst.line "GroupData") nodes
                newMethod("GetGroup", parser.AsKind(), [("name", Kind "string")], body)
            { constructor with
                Doc = [
                    """<summary>Get data from a subgroup</summary>"""
                    """<param name="name">Name of the subgroup</param>"""
                ]
            })
        // Getters: list of objects of each type
        for (name, valueType, ptyp) in topComplexTypes do
            parser.AddMember(
                let body =
                    MyAst.node 0 [
                        MyAst.line "this.Items"
                        MyAst.line "|> Seq.collect (fun data -> data.GetLeaves())"
                        MyAst.line $"|> Seq.choose (fun (name, value) -> if name = \"{name}\" then Some ({ptyp.Name}(value)) else None)"
                    ]
                newProperty(sprintf "ListOf%s" name, Kind $"{ptyp.Name} seq", body))
        // Get the flattened list of objects as instances of McuBase and its subtypes, when appropriate
        parser.AddMember(buildAsMcuList logInfo namedValueTypes)
        // Logging
        logInfo "Done building the group parser type"
        // Return result
        parser

/// Entry point of the type provider.
let generateCode(enableLogging : bool, nsName : string, sample : string) =

    let logInfo, closeLog =
        if enableLogging then
            let logger = Logging.initLogging() |> Result.bind Logging.openLogFile
            Logging.log logger, fun() -> Logging.closeLog logger
        else
            (fun _ -> ()), (fun () -> ())

    // The types corresponding to the ValueTypes extracted from the sample file
    let getProvidedType, cache = Internal.mkProvidedTypeBuilder logInfo
    let types, _ = AutoSchema.getTopTypes(Parsing.Stream.FromFile(sample))
    // Add top types
    let topTypeDefs =
        types
        |> Seq.map (fun kvp -> getProvidedType { Name = kvp.Key; Kind = kvp.Value; Parents = [] })
        |> List.ofSeq
    // Add ground types
    let groundTypeDefs =
        Seq.zip Ast.groundValueTypes Ast.groundValueTypeNames
        |> Seq.map (fun (kind, name) -> getProvidedType { Name = name; Kind = kind; Parents = [] })
        |> List.ofSeq
    // All types, but filter out cache entries for fields with ground types
    let namedTypes =
        cache
        |> Seq.choose (fun kvp ->
            let typId = kvp.Key
            match typId.Kind with
            | Ast.NameOfGroundType(name) ->
                if name = typId.Name then
                    Some(typId.Name, typId.Kind, fst kvp.Value)
                else
                    None
            | _ ->
                Some(typId.Name, typId.Kind, fst kvp.Value))
        |> List.ofSeq
    // Types that can appear at the top level of mission files
    let topComplexTypes =
        cache
        |> Seq.choose (fun kvp ->
            let typId = kvp.Key
            match typId with
            | { Kind = Ast.ValueType.Composite _; Parents = [] } ->
                Some(typId.Name, typId.Kind, kvp.Value)
            | _ ->
                None)
        |> List.ofSeq
    // The type of the file parser.
    let parserType =
        let topComplexTypes =
            topComplexTypes
            |> List.map (fun (name, kind, v) -> (name, kind, fst v))
        Internal.buildGroupParserType logInfo namedTypes topComplexTypes
    seq {
        yield $"""
namespace {nsName}

open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.Ast
open MBrace.FsPickler

/// The base type of all provided types representing objects found in a mission file, wraps an Ast.Value
type AstValueWrapper(value : Ast.Value) =
    member this.Wrapped = value

/// The base type of the result of parsing a mission file
type GroupMembers(items : SturmovikMission.DataProvider.Ast.Data list) =
    member this.Items = items

    """

        for (groundType, _) in groundTypeDefs do
            yield! groundType.Ast.Lines()

        for (_, _, (topType, modul)) in topComplexTypes do
            match modul with
            | Some modul ->
                if modul.Content.Count > 0 then
                    yield! modul.Ast.Lines()
            | None ->
                ()
            yield! topType.Ast.Lines()

        yield! parserType.Ast.Lines()
    }
    |> String.concat ""
