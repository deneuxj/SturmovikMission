//    Copyright 2015, 2020 Johann Deneux
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


namespace SturmovikMission.DataProvider.TypeProvider

#nowarn "25" // Incomplete pattern matches, occurs a lot due to "fun [this] -> <@@ ... @@>"

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open System.Collections.Generic
open System
open System.IO
open SturmovikMission.Cached
open SturmovikMission.DataProvider.UniqueNames
open SturmovikMission.DataProvider

module internal Internal =
    type IProvidedDataBuilder =
        /// <summary>
        /// Build a ProvidedConstructor.
        /// </summary>
        /// <param name="args">Argument names and types.</param>
        /// <param name="body">The code to execute in the constructor.</param>
        abstract NewConstructor: args: ((string * Type) list) * body: (Expr list -> Expr) -> ProvidedConstructor
        /// <summary>
        /// Build a ProvidedProperty.
        /// </summary>
        /// <param name="name">Name of the property.</param>
        /// <param name="typ">Type of the property.</param>
        /// <param name="body">Code of the property.</param>
        abstract NewProperty: name: string * typ: Type * body: (Expr -> Expr) -> ProvidedProperty
        /// Build a static ProvidedProperty.
        abstract NewStaticProperty: name: string * typ: Type * body: Expr -> ProvidedProperty
        /// <summary>
        /// Build a ProvidedMethod.
        /// </summary>
        /// <param name="name">Name of the method.</param>
        /// <param name="typ">Type of returned value.</param>
        /// <param name="args">Arguments to the method with their respective types.</param>
        /// <param name="body">Body of the method.</param>
        abstract NewMethod: name: string * typ: Type * args: (string * Type) list * body: (Expr list -> Expr) -> ProvidedMethod
        /// Build a static ProvidedMethod.
        abstract NewStaticMethod: name: string * typ: Type * args : (string * Type) list * body: (Expr list -> Expr) -> ProvidedMethod

    /// <summary>Controls the expression used for the InvokeCode of provided types</summary>
    /// Visual Studio and possibly other IDEs process expressions in InvokeCodes rather often.
    /// This can take enough time to render intellisense very sluggish.
    /// Considering that IDEs typically do not need the bodies of provided types,
    /// we let the user control whether such expressions should be assigned to InvokeCodes,
    /// or "empty failwith shells" should be used instead.
    /// There are three alternatives, two of which excplicitly specify what to do (FailWith and AsProvided),
    /// and the third lets the implementation guess according to the entry assembly (FromAssembly).
    type InvokeCodeImplementation =
        | FailWith = 0
        | AsProvided = 1
        | FromAssembly = 2

    let mkProvidedDataBuilder (invokeImpl : InvokeCodeImplementation) =
        let bodyGate =
            match invokeImpl with
            | InvokeCodeImplementation.FailWith ->
                fun _ -> <@@ failwith "Expressions replaced by shells" @@>
            | InvokeCodeImplementation.AsProvided ->
                id
            | InvokeCodeImplementation.FromAssembly ->
                // Dirty method that guesses the correct thing to do from the entry assembly and process.
                let asm = System.Reflection.Assembly.GetEntryAssembly()
                if asm = null then
                    let proc = System.Diagnostics.Process.GetCurrentProcess()
                    match proc.ProcessName with
                    | "Fsi" -> // Fsi running in an unmanaged process (such as Visual Studio)
                        id // Use the provided code.
                    | name -> // Some unmanaged process (such as Visual Studio)
                        let name = Expr.Value name
                        fun _ -> <@@ failwithf "Expressions replaced by shells when code was generated with %s" %%name @@> // Replace by empty shells
                else // Some managed process (such as the F# compiler)
                    id
            | _ ->
                failwith "Unexpected InvokeCodeImplementation"

        let funcGate =
            match invokeImpl with
            | InvokeCodeImplementation.FailWith ->
                fun _ -> fun _ -> <@@ failwith "Functions replaced by shells" @@>
            | InvokeCodeImplementation.AsProvided ->
                id
            | InvokeCodeImplementation.FromAssembly ->
                // Dirty method that guesses the correct thing to do from the entry assembly and process.
                let asm = System.Reflection.Assembly.GetEntryAssembly()
                if asm = null then
                    let proc = System.Diagnostics.Process.GetCurrentProcess()
                    match proc.ProcessName with
                    | "Fsi" -> // Fsi running in an unmanaged process (such as Visual Studio)
                        id // Use the provided code.
                    | name -> // Some unmanaged process (such as Visual Studio)
                        let name = Expr.Value name
                        fun _ -> fun _ -> <@@ failwithf "Functions replaced by shells when code was generated by %s" %%name @@> // Replace by empty shells
                else // Some managed process (such as the F# compiler)
                    id
            | _ ->
                failwith "Unexpected InvokeCodeImplementation"

        let newConstructor (args : (string * Type) list) (body : Expr list -> Expr) =
            let args =
                args
                |> List.map (fun (n, t) -> ProvidedParameter(n, t))
            let cnstr =
                ProvidedConstructor(args, funcGate body)
            cnstr

        let newProperty (name : string, typ : Type) (body : Expr -> Expr) =
            let prop = ProvidedProperty(name, typ, fun [this] -> bodyGate(body this))
            prop

        let newStaticProperty (name : string, typ : Type) (body : Expr) =
            let prop = ProvidedProperty(name, typ, getterCode = (fun [] -> bodyGate body), isStatic = true)
            prop

        let newMethod (name : string, typ : Type) (args : (string * Type) list) (body : Expr list -> Expr) =
            let args =
                args
                |> List.map (fun (n, t) -> ProvidedParameter(n, t))
            let m = ProvidedMethod(name, args, typ, funcGate body)
            m

        let newStaticMethod (name : string, typ : Type) (args : (string * Type) list) (body : Expr list -> Expr) =
            let args =
                args
                |> List.map (fun (n, t) -> ProvidedParameter(n, t))
            let m = ProvidedMethod(name, args, typ, funcGate body, isStatic = true)
            m

        { new IProvidedDataBuilder with
            member __.NewConstructor(args, body) = newConstructor args body
            member __.NewProperty(name, typ, body) = newProperty (name, typ) body
            member __.NewStaticProperty(name, typ, body) = newStaticProperty (name, typ) body
            member __.NewMethod(name, typ, args, body) = newMethod (name, typ) args body
            member __.NewStaticMethod(name, typ, args, body) = newStaticMethod (name, typ) args body
        }

    /// Add documentation to a provided method, constructor, property or type definition.
    let inline addXmlDoc< ^T when ^T: (member AddXmlDoc : string -> unit)> (doc : string) (thing : ^T) : ^T =
        ( ^T : (member AddXmlDoc : string -> unit) (thing, doc))
        thing

    /// <summary>
    /// Type of keys using in caching provided type definitions.
    /// </summary>
    type TypeIdentification = {
        Name : string
        Kind : Ast.ValueType
        Parents : string list
    }

    /// <summary>
    /// Build the function that builds ProvidedTypeDefinitions for ValueTypes encountered in the sample mission file.
    /// </summary>
    /// <param name="top">Definition of the top type in the type provider.</param>
    let mkProvidedTypeBuilder logInfo (pdb : IProvidedDataBuilder) (top : ProvidedTypeDefinition) =
        logInfo "Started inferring all MCU value types"

        let cache = new Dictionary<TypeIdentification, ProvidedTypeDefinition>(HashIdentity.Structural)

        let asList this = <@ (%%this : Ast.Value).GetItems() @>

        // Builders for the ground types

        let ptypBoolean =
            let ptyp =
                new ProvidedTypeDefinition("Boolean", Some (typeof<Ast.Value>))
            ptyp.AddMember(pdb.NewProperty("Value", typeof<bool>, fun this -> <@@ (%%this : Ast.Value).GetBool() @@>))
            ptyp.AddMember(pdb.NewConstructor([("Value", typeof<bool>)], fun [value] -> <@@ Ast.Value.Boolean (%%value : bool) @@>))
            ptyp

        let ptypFloat =
            let ptyp =
                new ProvidedTypeDefinition("Float", Some (typeof<Ast.Value>))
            ptyp.AddMember(pdb.NewProperty("Value", typeof<float>, fun this -> <@@ (%%this : Ast.Value).GetFloat() @@>))
            ptyp.AddMember(pdb.NewConstructor([("Value", typeof<float>)], fun [value] -> <@@ Ast.Value.Float (%%value : float) @@>))
            ptyp

        let ptypFloatPair =
            let ptyp =
                new ProvidedTypeDefinition("FloatPair", Some (typeof<Ast.Value>))
            ptyp.AddMember(pdb.NewProperty("Value", typeof<float * float>, fun this -> <@@ (%%this : Ast.Value).GetFloatPair() @@>))
            ptyp.AddMember(pdb.NewConstructor([("Value", typeof<float * float>)], fun [value] -> <@@ let x, y = (%%value : float * float) in Ast.Value.FloatPair(x, y) @@>))
            ptyp

        let ptypInteger =
            let ptyp =
                new ProvidedTypeDefinition("Integer", Some (typeof<Ast.Value>))
            ptyp.AddMember(pdb.NewProperty("Value", typeof<int>, fun this -> <@@ (%%this : Ast.Value).GetInteger() @@>))
            ptyp.AddMember(pdb.NewConstructor([("Value", typeof<int>)], fun [value] -> <@@ Ast.Value.Integer (%%value : int) @@>))
            ptyp

        let ptypString =
            let ptyp =
                new ProvidedTypeDefinition("String", Some (typeof<Ast.Value>))
            ptyp.AddMember(pdb.NewProperty("Value", typeof<string>, fun this -> <@@ (%%this : Ast.Value).GetString() @@>))
            ptyp.AddMember(pdb.NewConstructor([("Value", typeof<string>)], fun [value] -> <@@ Ast.Value.String (%%value : string) @@>))
            ptyp

        let ptypIntVector =
            let ptyp =
                new ProvidedTypeDefinition("VectorOfIntegers", Some (typeof<Ast.Value>))
            ptyp.AddMember(pdb.NewProperty("Value", typeof<int list>, fun this -> <@@ (%%this : Ast.Value).GetIntVector() @@>))
            ptyp.AddMember(pdb.NewConstructor([("Value", typeof<int list>)], fun [value] -> <@@ Ast.Value.IntVector (%%value : int list) @@>))
            ptyp

        let ptypDate =
            let ptyp =
                new ProvidedTypeDefinition("Date", Some (typeof<Ast.Value>))
            ptyp.AddMember(pdb.NewProperty("Year", typeof<int>, fun this ->
                let e = <@@ (%%this : Ast.Value).GetDate() @@>
                <@@ let _, _, year = (%%e : int * int * int) in year @@>))
            ptyp.AddMember(pdb.NewProperty("Month", typeof<int>, fun this ->
                let e = <@@ (%%this : Ast.Value).GetDate() @@>
                <@@ let _, month, _ = (%%e : int * int * int) in month @@>))
            ptyp.AddMember(pdb.NewProperty("Day", typeof<int>, fun this ->
                let e = <@@ (%%this : Ast.Value).GetDate() @@>
                <@@ let day, _, _ = (%%e : int * int * int) in day @@>))
            ptyp.AddMember(pdb.NewConstructor([("Day", typeof<int>); ("Month", typeof<int>); ("Year", typeof<int>)], fun [day; month; year] ->
                <@@ Ast.Value.Date((%%day : int), (%%month : int), (%%year : int)) @@>))
            ptyp

        let addComplexNestedType(ptyp : ProvidedTypeDefinition, subpTyp : ProvidedTypeDefinition, kind : Ast.ValueType) =
            match kind with
            | Ast.ValueType.Boolean
            | Ast.ValueType.Float
            | Ast.ValueType.FloatPair
            | Ast.ValueType.Integer
            | Ast.ValueType.String
            | Ast.ValueType.IntVector
            | Ast.ValueType.Date -> () // Ground type, do nothing
            | _ ->
                // Complex type, add it
                match ptyp.GetMember(subpTyp.Name) with
                | [||] ->
                    ptyp.AddMember(subpTyp)
                | _ ->
                    failwithf "Cannot add provided type for '%s' in '%s', there is already a member by that name." subpTyp.Name ptyp.Name

        // Build any kind of type, ground or complex.
        let rec buildProvidedType (typId : TypeIdentification) =
            logInfo <| sprintf "Started building provided type for %s" typId.Name
            try
                let name = typId.Name
                let ptyp =
                    match typId.Kind with
                    | Ast.ValueType.Boolean -> ptypBoolean
                    | Ast.ValueType.Float -> ptypFloat
                    | Ast.ValueType.FloatPair -> ptypFloatPair
                    | Ast.ValueType.Integer -> ptypInteger
                    | Ast.ValueType.String -> ptypString
                    | Ast.ValueType.IntVector -> ptypIntVector
                    | Ast.ValueType.Date -> ptypDate
                    | Ast.ValueType.Pair (typ1, typ2) -> buildPair(typId, typ1, typ2)
                    | Ast.ValueType.Triplet (typ1, typ2, typ3) -> buildTriple(typId, typ1, typ2, typ3)
                    | Ast.ValueType.Composite fields ->
                        let typExpr = typId.Kind.ToExpr()
                        let ptyp =
                            new ProvidedTypeDefinition(name, Some (typeof<Ast.Value>))
                        let parents = name :: typId.Parents
                        // Add types of complex fields as nested types
                        for field in fields do
                            let fieldName = field.Key
                            let fieldKind, _, _ = field.Value
                            let subpTyp = getProvidedType { Name = fieldName; Kind = fieldKind; Parents = parents }
                            addComplexNestedType(ptyp, subpTyp, fieldKind)
                        // Constructor
                        match construct parents (fields, ptyp) with
                        | None -> ()
                        | Some cstr -> ptyp.AddMemberDelayed(fun() -> cstr)
                        // Getters
                        ptyp.AddMembersDelayed(fun() -> getters parents fields)
                        // Setters
                        ptyp.AddMembersDelayed(fun() -> setters parents (fields, ptyp))
                        // Create as MCU
                        ptyp.AddMembersDelayed(fun() -> asMcu (name, typId.Kind, typExpr))
                        // Parse
                        ptyp.AddMemberDelayed(fun() -> staticParser (typId.Kind, name, ptyp))
                        // Dump to text
                        let meth = pdb.NewMethod("AsString", typeof<string>, [], fun [this] ->
                            <@@
                                sprintf "%s %s" name (Ast.dump (%%this : Ast.Value))
                            @@>)
                        ptyp.AddMember(meth)
                        // Result
                        ptyp
                    | Ast.ValueType.Mapping itemTyp ->
                        let subName = sprintf "%s_ValueType" name
                        let ptyp1 = getProvidedType { Name = subName; Kind = itemTyp; Parents = name :: typId.Parents }
                        let ptyp =
                            new ProvidedTypeDefinition(name, Some (typeof<Ast.Value>))
                        addComplexNestedType(ptyp, ptyp1, itemTyp)
                        // Constructor from map
                        ptyp.AddMember(pdb.NewConstructor([("map", typedefof<Map<_, _>>.MakeGenericType(typeof<int>, ptyp1))], fun [m] ->
                            <@@
                                let m = (%%m : Map<int, Ast.Value>)
                                Ast.Value.Mapping(Map.toList m)
                            @@>))
                        // Value getter
                        let propTyp = typedefof<Map<_,_>>.MakeGenericType(typeof<int>, ptyp1)
                        ptyp.AddMember(pdb.NewProperty("Value", propTyp, fun this -> <@@ (%%this : Ast.Value).GetMapping() |> Map.ofList @@>))
                        // Set item in the map
                        ptyp.AddMember(pdb.NewMethod("SetItem", ptyp, [("Key", typeof<int>); ("Value", upcast ptyp1)], fun [this; key; value] ->
                            <@@
                                let this = (%%this : Ast.Value)
                                this.SetItem((%%key : int), (%%value : Ast.Value))
                            @@>))
                        // Remove item from the map
                        ptyp.AddMember(pdb.NewMethod("RemoveItem", ptyp, ["Key", typeof<int>], fun [this; key] ->
                            <@@
                                let this = (%%this : Ast.Value)
                                this.RemoveItem(%%key : int)
                            @@>))
                        // Clear map
                        ptyp.AddMember(pdb.NewMethod("Clear", ptyp, [], fun [this] ->
                            <@@
                                let this = (%%this : Ast.Value)
                                this.Clear()
                            @@>))
                        // Result
                        ptyp
                    | Ast.ValueType.List itemTyp ->
                        let subName = sprintf "%s_ValueType" name
                        let ptyp1 = getProvidedType { Name = subName; Kind = itemTyp; Parents = name :: typId.Parents }
                        let ptyp =
                            new ProvidedTypeDefinition(name, Some (typeof<Ast.Value>))
                        addComplexNestedType(ptyp, ptyp1, itemTyp)
                        // Value getter
                        let propTyp = typedefof<_ list>.MakeGenericType(ptyp1)
                        ptyp.AddMember(pdb.NewProperty("Value", propTyp, fun this -> <@@ (%%this : Ast.Value).GetList() @@>))
                        // constructor with value
                        ptyp.AddMember(pdb.NewConstructor(["items", propTyp], fun [items] -> <@@ Ast.Value.List (%%items : Ast.Value list)@@>))
                        // Result
                        ptyp
                // Add a default constructor
                let defaultValue = Ast.defaultExprValue typId.Kind
                ptyp.AddMember(pdb.NewConstructor([], fun [] -> defaultValue.Raw))
                ptyp
            finally
                logInfo <| sprintf "Done building provided type for %s" typId.Name

        and buildPair (typId : TypeIdentification, typ1 : Ast.ValueType, typ2 : Ast.ValueType) =
            let ptyp1 =
                let subName = sprintf "%s_ValueType1" typId.Name
                getProvidedType { Name = subName; Kind = typ1; Parents = typId.Name :: typId.Parents }
            let ptyp2 =
                let subName = sprintf "%s_ValueType2" typId.Name
                getProvidedType { Name = subName; Kind = typ2; Parents = typId.Name :: typId.Parents }
            let ptyp =
                new ProvidedTypeDefinition(typId.Name, Some (typeof<Ast.Value>))
            addComplexNestedType(ptyp, ptyp1, typ1)
            addComplexNestedType(ptyp, ptyp2, typ2)
            // Value getter
            let propTyp = typedefof<_*_>.MakeGenericType(ptyp1, ptyp2)
            ptyp.AddMember(pdb.NewProperty("Value", propTyp, fun this -> <@@ (%%this : Ast.Value).GetPair() @@>))
            // Constructor
            ptyp.AddMember(pdb.NewConstructor([("Value", propTyp)], fun [value] -> <@@ Ast.Value.Pair (%%value : Ast.Value * Ast.Value) @@>))
            // Result
            ptyp

        and buildTriple (typId : TypeIdentification, typ1 : Ast.ValueType, typ2 : Ast.ValueType, typ3 : Ast.ValueType) =
            let name = typId.Name
            let ptyp1 =
                let subName = sprintf "%s_ValueType1" name
                getProvidedType { Name = subName; Kind = typ1; Parents = name :: typId.Parents }
            let ptyp2 =
                let subName = sprintf "%s_ValueType2" name
                getProvidedType { Name = subName; Kind = typ2; Parents = name :: typId.Parents }
            let ptyp3 =
                let subName = sprintf "%s_ValueType3" name
                getProvidedType { Name = subName; Kind = typ3; Parents = name :: typId.Parents }
            let ptyp =
                new ProvidedTypeDefinition(name, Some (typeof<Ast.Value>))
            addComplexNestedType(ptyp, ptyp1, typ1)
            addComplexNestedType(ptyp, ptyp2, typ2)
            addComplexNestedType(ptyp, ptyp3, typ3)
            let propTyp = typedefof<_*_*_>.MakeGenericType(ptyp1, ptyp2, ptyp3)
            // Value getter
            ptyp.AddMember(pdb.NewProperty("Value", propTyp, fun this -> <@@ (%%this : Ast.Value).GetTriplet() @@>))
            // Constructor
            ptyp.AddMember(pdb.NewConstructor([("Value", propTyp)], fun [value] -> <@@ Ast.Value.Triplet (%%value : Ast.Value * Ast.Value * Ast.Value) @@>))
            // Result
            ptyp

        // Build the getters in composite types
        and getters parents fields =
            fields
            |> Map.map (
                fun fieldName (def, minMult, maxMult) ->
                    let fieldType =
                        getProvidedType { Name = fieldName; Kind = def; Parents = parents }
                    match (minMult, maxMult) with
                    | Ast.MinMultiplicity.MinOne, Ast.MaxMultiplicity.MaxOne ->
                        pdb.NewMethod(
                            sprintf "Get%s" fieldName,
                            fieldType,
                            [],
                            fun [this] ->
                                let e = asList this
                                <@@
                                    match List.tryFind (fun (name, _) -> name = fieldName) %e with
                                    | Some (_, value) -> value
                                    | None -> failwithf "Field '%s' is not set" fieldName
                                @@>)
                    | Ast.MinMultiplicity.Zero, Ast.MaxOne ->
                        let optTyp =
                            typedefof<_ option>
                                .MakeGenericType(fieldType)
                        pdb.NewMethod(
                            sprintf "TryGet%s" fieldName,
                            optTyp,
                            [],
                            fun [this] ->
                                let e = asList this
                                <@@
                                    match List.tryFind (fun (name, _) -> name = fieldName) %e with
                                    | Some (_, value) -> Some value
                                    | None -> None
                                @@>)
                    | _, Ast.MaxMultiplicity.Multiple ->
                        let listTyp =
                            typedefof<_ list>
                                .MakeGenericType(fieldType)
                        pdb.NewMethod(
                            sprintf "Get%ss" fieldName,
                            listTyp,
                            [],
                            fun [this] ->
                                let e = asList this
                                <@@
                                    List.filter (fun (name, _) -> name = fieldName) %e
                                    |> List.map snd
                                @@>)
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
                let fieldType =
                    getProvidedType { Name = fieldName; Kind = def; Parents = parents }
                match (minMult, maxMult) with
                | Ast.MinMultiplicity.MinOne, Ast.MaxMultiplicity.MaxOne ->
                    pdb.NewMethod(
                        sprintf "Set%s" fieldName,
                        ptyp,
                        [("value", upcast fieldType)],
                        fun [this; value] ->
                            <@@
                                let this = (%%this : Ast.Value)
                                this.SetItem(fieldName, (%%value : Ast.Value))
                            @@>)
                | Ast.MinMultiplicity.Zero, Ast.MaxOne ->
                    let optTyp =
                        typedefof<_ option>
                            .MakeGenericType(fieldType)
                    pdb.NewMethod(
                        sprintf "Set%s" fieldName,
                        ptyp,
                        [("value", optTyp)],
                        fun [this; value] ->
                            <@@
                                let this = (%%this : Ast.Value)
                                this.SetItem(fieldName, (%%value : Ast.Value option))
                            @@>)
                | _, Ast.MaxMultiplicity.Multiple ->
                    let listTyp =
                        typedefof<_ list>
                            .MakeGenericType(fieldType)
                    pdb.NewMethod(
                        sprintf "Set%s" fieldName,
                        ptyp,
                        [("value", listTyp)],
                        fun [this; value] ->
                            <@@
                                let this = (%%this : Ast.Value)
                                this.ClearItems(fieldName).AddItems(fieldName, (%%value : Ast.Value list))
                            @@>)
                |> addXmlDoc (sprintf """<summary>Create a copy of this, with the value of field '%s' changed to <paramref name="value" />.</summary>""" fieldName))
            |> List.ofSeq

        // Methods to build mutable MCU instances
        and asMcu (name, typ, typExpr) =
            [
                match McuFactory.tryMakeMcu(name, typ) with
                | Some _ ->
                    yield
                        pdb.NewMethod(
                            "CreateMcu",
                            typeof<Mcu.McuBase>,
                            [],
                            fun [this] ->
                                <@@
                                    match McuFactory.tryMakeMcu(name, %typExpr) with
                                    | Some f -> f((%%this : Ast.Value), [])
                                    | None -> failwith "Unexpected error: could not build MCU"
                                @@>)
                        |> addXmlDoc """<summary>Create a new mutable instance of an MCU.</summary>"""
                | None -> ()
            ]

        // Constructor. Its arguments are built similarly to the setters
        and construct parents (fields, ptyp) =
            let args =
                fields
                |> Seq.choose(fun kvp ->
                    let fieldName = kvp.Key
                    let (def, minMult, maxMult) = kvp.Value
                    let fieldType =
                        getProvidedType { Name = fieldName; Kind = def; Parents = parents }
                    match (minMult, maxMult) with
                    | Ast.MinMultiplicity.MinOne, Ast.MaxMultiplicity.MaxOne ->
                        Some (fieldName, fieldType :> Type)
                    | Ast.MinMultiplicity.Zero, Ast.MaxOne ->
                        None
                    | _, Ast.MaxMultiplicity.Multiple ->
                        None)
                |> List.ofSeq
            let argNames =
                args
                |> Seq.map fst
                |> Seq.fold (fun expr name -> <@ name :: %expr@>) <@ [] @>
            let body (args : Expr list) =
                let args =
                    args
                    |> List.fold (fun expr arg -> <@ (%%arg : Ast.Value) :: %expr @>) <@ [] @>
                <@@
                    Ast.Value.Composite(List.zip %argNames %args)
                @@>
            match args with
            | [] ->
                None
            | args ->
                pdb.NewConstructor(args, body)
                |> Some

        // static method to create a parser
        and staticParser (valueType : Ast.ValueType, name, ptyp) =
            let vtExpr = valueType.ToExpr()
            let retType = typeof<Parsing.ParserFun>

            pdb.NewStaticMethod("GetParser", retType, [], fun [] ->
                let name = Expr.Value name
                <@@
                    let bodyParser = Parsing.makeParser %vtExpr
                    fun (s : Parsing.Stream) ->
                        let (Parsing.SubString(data, offset)) = s
                        if data.Substring(offset).StartsWith((%%name : string)) then
                            let s = Parsing.SubString(data, offset + (%%name : string).Length)
                            bodyParser.Run s
                        else
                            Parsing.parseError(sprintf "Expected '%s'" %%name, s)
                    |> Parsing.ParserFun
                @@>)
            |> addXmlDoc ("""
            <summary>Create a parser for that type</summary>
            <return>A <c>Parsing.ParserFun</c> value</return>
            """)

        and getProvidedType typId : ProvidedTypeDefinition =
            cached cache buildProvidedType typId

        logInfo "Done inferring all MCU value types"

        getProvidedType, cache

    /// <summary>
    /// Build the provided method that builds a list of objects implementing McuBase and its subtypes.
    /// </summary>
    /// <param name="namedValueTypes">List of ValueTypes with their name and their provided type definition.</param>
    let buildAsMcuList logInfo (pdb : IProvidedDataBuilder) (namedValueTypes : (string * Ast.ValueType * ProvidedTypeDefinition) list) =
        logInfo "Started building the MCU list builder method"
        let valueTypeOfName =
            namedValueTypes
            |> List.fold (fun expr (name, valueType, _) ->
                <@
                    Map.add name %(valueType.ToExpr()) %expr
                @>
                ) <@ Map.empty @>
        let names =
            namedValueTypes
            |> List.map (fun (name, _, _) -> name)
            |> List.fold (fun expr name ->
                <@ name :: %expr @>) <@ [] @>

        let method =
            pdb.NewMethod("CreateMcuList", typeof<Mcu.McuBase list>, [], fun [this] ->
                <@@
                let this = (%%this : Ast.Data list)
                let valueTypeOfName = %valueTypeOfName
                let mcuMakerOfName =
                    %names
                    |> List.map (fun name ->
                        let valueType = valueTypeOfName.[name]
                        (name, McuFactory.tryMakeMcu(name, valueType))
                    )
                    |> Map.ofList
                this
                |> List.map (fun data -> data.GetLeavesWithPath())
                |> List.concat
                |> List.choose (fun (path, name, value) ->
                    match Map.tryFind name mcuMakerOfName with
                    | Some(Some(make)) ->
                        make(value, path)
                        |> Some
                    | _ -> // Cannot build an Mcu from that valueType
                        None
                )
                @@>
            )

        logInfo "Done building the MCU list builder method"

        method

    /// <summary>
    /// Build the provided type definition of the type that offers parsing of mission files.
    /// </summary>
    /// <param name="namedValueTypes">ValueTypes with their name and provided type definition.</param>
    let buildGroupParserType logInfo (pdb : IProvidedDataBuilder) (namedValueTypes : (string * Ast.ValueType * ProvidedTypeDefinition) list) (topComplexTypes : (string * Ast.ValueType * ProvidedTypeDefinition) list) =
        logInfo "Started building the group parser type"
        let parser =
            ProvidedTypeDefinition("GroupData", Some typeof<Ast.Data list>)
            |> addXmlDoc """Extraction of data from a mission or group file."""
        let valueTypeOfName =
            namedValueTypes
            |> List.fold (fun expr (name, valueType, _) ->
                <@
                    Map.add name %(valueType.ToExpr()) %expr
                @>
                ) <@ Map.empty @>
        // Constructor: Parse a group or mission file
        parser.AddMemberDelayed(fun() ->
            pdb.NewConstructor([("s", typeof<Parsing.Stream>)], fun [s] ->
                <@@
                    let parsers =
                        %valueTypeOfName
                        |> Map.map (fun name valueType -> Parsing.makeParser valueType)
                    let getParser name = parsers.[name]
                    let s = (%%s : Parsing.Stream)
                    Parsing.parseFile getParser s
                @@>)
            |> addXmlDoc """
                <summary>Parse a mission or group file and store the extracted data.</summary>
                <param name="s">The stream that is parsed</param>
                <exception cref="Parsing.ParseError">Failed to parse the mission or group</exception>""")
        // Constructor: From a list of AST nodes
        parser.AddMemberDelayed(fun () ->
            pdb.NewConstructor([("nodes", typeof<Ast.Data list>)], fun [nodes] ->
                <@@
                    (%%nodes : Ast.Data list)
                @@>)
            |> addXmlDoc """
                <summary>Provide access to parsed data.</summary>
                <param name="nodes">The result of parsing a group or mission file</param>""")
        // Get data from a subgroup
        parser.AddMemberDelayed(fun () ->
            pdb.NewMethod("GetGroup", parser, [("name", typeof<string>)], fun [this; name] ->
                <@@
                    let nodes = (%%this : Ast.Data list)
                    nodes
                    |> List.map (fun node -> node.FindByPath [(%%name : string)])
                    |> List.concat
                @@>
            )
            |> addXmlDoc """
                <summary>Get data from a subgroup</summary>
                <param name="name">Name of the subgroup</param>""")
        // Getters: list of objects of each type
        for (name, valueType, ptyp) in topComplexTypes do
            parser.AddMemberDelayed(fun() ->
                pdb.NewProperty(sprintf "ListOf%s" name, typedefof<_ list>.MakeGenericType(ptyp), fun this ->
                    <@@
                        let this = (%%this : Ast.Data list)
                        let ret =
                            this
                            |> List.map (fun data -> data.GetLeaves())
                            |> List.concat
                            |> List.choose (function (name2, value) -> if name2 = name then Some value else None)
                        ret
                    @@>)
                |> addXmlDoc (sprintf """<summary>Build a list of immutable instances of %s</summary>""" name))
        // Get the flattened list of objects as instances of McuBase and its subtypes, when appropriate
        parser.AddMemberDelayed(fun() ->
            buildAsMcuList logInfo pdb namedValueTypes
            |> addXmlDoc """<summary>Build a list of mutable instances of McuBase from the extracted data.</summary>""")
        // Logging
        logInfo "Done building the group parser type"
        // Return result
        parser

    /// <summary>
    /// Start a background async that waits for a file to change
    /// </summary>
    /// <param name="path">Path to file to watch</param>
    /// <param name="signal">Action to perform when the watched file changes</param>
    let watchFile (path : string) signal =
        let watcher = new FileSystemWatcher(Path.GetDirectoryName(path), Path.GetFileName(path))
        watcher.NotifyFilter <- NotifyFilters.LastWrite
        let wait =
            async {
                let! change = Async.AwaitEvent watcher.Changed
                signal()
                watcher.Dispose()
            }
        watcher.EnableRaisingEvents <- true
        Async.Start wait

[<TypeProvider>]
/// Entry point of the type provider.
type MissionTypes(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("SturmovikMission.DataProvider.DesignTime", "SturmovikMission.DataProvider.Runtime")], addDefaultProbingLocation=true)
    
    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "SturmovikMissionTypes"

    // check we contain a copy of runtime files, and are not referencing the runtime DLL
    do assert (typeof<Parsing.Stream>.Assembly.GetName().Name = asm.GetName().Name)  

    let provider = ProvidedTypeDefinition(asm, ns, "Provider", Some(typeof<obj>))
    do provider.AddXmlDoc("""
    <summary>
    Exposes typed data from "IL2 Sturmovik: Battle of Stalingrad" mission files.
    </summary>
    <param name="sample">
    Name of a mission file from which the structure of missions is infered.
    </param>
    <param name="enableLogging">
    Control whether calls to type provider are logged to files in %LocalAppData%/SturmovikMission.DataProvider.
    False (logging disabled) by default
    </param>
    """)
    let sampleParam = ProvidedStaticParameter("sample", typeof<string>)
    let enableLoggingParam = ProvidedStaticParameter("enableLogging", typeof<bool>, parameterDefaultValue = false)

    let buildProvider (enableLogging : bool) (typeName : string, sample : string) =
        let logInfo, closeLog =
            if enableLogging then
                let logger = Logging.initLogging() |> Result.bind Logging.openLogFile
                Logging.log logger, fun() -> Logging.closeLog logger
            else
                (fun _ -> ()), (fun () -> ())

        let invokeImpl =
            if config.IsHostedExecution then
                Internal.InvokeCodeImplementation.AsProvided
            else
                Internal.InvokeCodeImplementation.FromAssembly

        logInfo(sprintf "Provider invoked with sample '%s' using invokeCodeImpl '%s'" sample (string invokeImpl))

        let pdb = Internal.mkProvidedDataBuilder invokeImpl
        let ty = new ProvidedTypeDefinition(asm, ns, typeName, Some(typeof<obj>))
        // The types corresponding to the ValueTypes extracted from the sample file
        let getProvidedType, cache = Internal.mkProvidedTypeBuilder logInfo pdb ty
        let types, _ = AutoSchema.getTopTypes(Parsing.Stream.FromFile(sample))
        // Add top types
        for t in types do
            let typeDef = getProvidedType { Name = t.Key; Kind = t.Value; Parents = [] }
            ty.AddMember(typeDef)
        // Add ground types
        for kind, name in Seq.zip Ast.groundValueTypes Ast.groundValueTypeNames do
            let typeDef = getProvidedType { Name = name; Kind = kind; Parents = [] }
            ty.AddMember(typeDef)
        // The type of the parser.
        let namedTypes =
            cache
            |> Seq.choose (fun kvp ->
                let typId = kvp.Key
                match typId.Kind with
                | Ast.NameOfGroundType(name) ->
                    if name = typId.Name then
                        Some(typId.Name, typId.Kind, kvp.Value)
                    else
                        None
                | _ ->
                    Some(typId.Name, typId.Kind, kvp.Value))
            |> List.ofSeq
        // The type of the file parser.
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
        let parserType = Internal.buildGroupParserType logInfo pdb namedTypes topComplexTypes
        ty.AddMember(parserType)
        // Resolution folder
        let resFolder = pdb.NewStaticProperty("ResolutionFolder", typeof<string>, Expr.Value(config.ResolutionFolder))
        resFolder.AddXmlDoc("""
        <summary>
        Location of the resolution folder, the folder used to root relative paths provided in the type provider's constructor.
        </summary>
        """)
        ty.AddMember(resFolder)
        // File modification times
        let modifs =
            [
                yield FileWithTime.File.FromFile sample
            ]
        // Result
        ty, modifs, closeLog

    // Cache the top provided type definitions.
    let cache = new Dictionary<(string * string), ProvidedTypeDefinition * (FileWithTime.File list) * (unit -> unit) >(HashIdentity.Structural)
    let getProvider logInfo = cached cache (buildProvider logInfo)

    do provider.DefineStaticParameters([sampleParam; enableLoggingParam], fun typeName [| sample; enableLogging |] ->
        let resolve (path : string) =
            if Path.IsPathRooted(path) then
                path
            else
                Path.Combine(config.ResolutionFolder, path)
        let sample = sample :?> string |> resolve
        let enableLogging = enableLogging :?> bool

        if not(System.IO.File.Exists(sample)) then
            failwithf "Cannot open sample file '%s' for reading (runtime assembly is '%s')" sample config.RuntimeAssembly
        // Check if modifications were made to input files
        let ty, modifs, closeLog = getProvider enableLogging (typeName, sample)
        let modifs2 =
            [
                yield FileWithTime.File.FromFile sample
            ]
        // If so, remove the entry from the cache, invalidate the top provided and build it again.
        if modifs <> modifs2 then
            closeLog()
            cache.Remove((typeName, sample)) |> ignore
            this.Invalidate()
            let ty, _, _ = getProvider enableLogging (typeName, sample)
            // Invalidate the type provider whenever the sample file or one of the library files is modified
            if config.IsInvalidationSupported then
                Internal.watchFile sample this.Invalidate
            ty
        else
            ty
    )

    do this.AddNamespace(ns, [provider])


[<assembly:TypeProviderAssembly>]
do
    ()