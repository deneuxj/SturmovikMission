﻿module SturmovikMission.DataProvider.TypeProvider

#nowarn "25" // Incomplete pattern matches, occurs a lot due to "fun [this] -> <@@ ... @@>"

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open System.Collections.Generic
open System

let cached (cache : IDictionary<'a, 'b>) f x =
    match cache.TryGetValue(x) with
    | true, y -> y
    | false, _ ->
        let y = f x
        cache.Add(x, y)
        y

let getNameStore getValidNames =
    let reserved =
        [ "Boolean"; "Float"; "Integer"; "String"; "VectorOfIntegers"; "Date" ]
    let store = ref <| Set reserved
    let isValid name =
        Set.contains name !store
        |> not
    fun baseName ->
        let name =
            getValidNames baseName
            |> Seq.find isValid
        store := Set.add name !store
        name

let addConstructor (ptyp : ProvidedTypeDefinition) (args : (string * Type) list) (body : Expr list -> Expr) =
    let args =
        args
        |> List.map (fun (n, t) -> ProvidedParameter(n, t))
    let cnstr =
        ProvidedConstructor(args)
    cnstr.InvokeCode <- body
    ptyp.AddMember(cnstr)

let addProperty (ptyp : ProvidedTypeDefinition) (name : string, typ : Type) (body : Expr -> Expr) =
    let prop = ProvidedProperty(name, typ)
    prop.GetterCode <- fun [this] -> body this
    ptyp.AddMember(prop)

let addMethod (ptyp : ProvidedTypeDefinition) (name : string, typ : Type) (args : (string * Type) list) (body : Expr list -> Expr) =
    let args =
        args
        |> List.map (fun (n, t) -> ProvidedParameter(n, t))
    let m = ProvidedMethod(name, args, typ)
    m.InvokeCode <- body
    ptyp.AddMember(m)

let addStaticMethod (ptyp : ProvidedTypeDefinition) (name : string, typ : Type) (args : (string * Type) list) (body : Expr list -> Expr) =
    let args =
        args
        |> List.map (fun (n, t) -> ProvidedParameter(n, t))
    let m = ProvidedMethod(name, args, typ)
    m.IsStaticMethod <- true
    m.InvokeCode <- body
    ptyp.AddMember(m)

    
let mkProvidedTypeBuilder(top : ProvidedTypeDefinition) =
    let newName =
        fun baseName ->
            Seq.initInfinite (fun i -> if i = 0 then baseName else sprintf "%s_%d" baseName (i + 1))
        |> getNameStore

    let cache = new Dictionary<string option * Ast.ValueType, ProvidedTypeDefinition>(HashIdentity.Structural)

    let rec buildProvidedType (name : string option, typ : Ast.ValueType) =
        let typExpr = typ.ToExpr()
        match typ with
        | Ast.ValueType.Boolean ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Boolean", Some (typeof<Ast.Value>))
            addProperty ptyp ("Value", typeof<bool>) (fun this -> <@@ (%%this : Ast.Value).GetBool() @@>)
            addConstructor ptyp [("Value", typeof<bool>)] (fun [value] -> <@@ Ast.Value.Boolean (%%value : bool) @@>)
            ptyp
        | Ast.ValueType.Float ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Float", Some (typeof<Ast.Value>))
            addProperty ptyp ("Value", typeof<float>) (fun this -> <@@ (%%this : Ast.Value).GetFloat() @@>)
            addConstructor ptyp [("Value", typeof<float>)] (fun [value] -> <@@ Ast.Value.Float (%%value : float) @@>)
            ptyp
        | Ast.ValueType.Integer ->
            let ptyp =
                new ProvidedTypeDefinition("Integer", Some (typeof<Ast.Value>))
            addProperty ptyp ("Value", typeof<int>) (fun this -> <@@ (%%this : Ast.Value).GetInteger() @@>)
            addConstructor ptyp [("Value", typeof<int>)] (fun [value] -> <@@ Ast.Value.Integer (%%value : int) @@>)
            ptyp
        | Ast.ValueType.String ->
            let ptyp =
                new ProvidedTypeDefinition("String", Some (typeof<Ast.Value>))
            addProperty ptyp ("Value", typeof<string>) (fun this -> <@@ (%%this : Ast.Value).GetString() @@>)
            addConstructor ptyp [("Value", typeof<string>)] (fun [value] -> <@@ Ast.Value.String (%%value : string) @@>)
            ptyp
        | Ast.ValueType.IntVector ->
            let ptyp =
                new ProvidedTypeDefinition("VectorOfIntegers", Some (typeof<Ast.Value>))
            addProperty ptyp ("Value", typeof<int list>) (fun this -> <@@ (%%this : Ast.Value).GetIntVector() @@>)
            addConstructor ptyp [("Value", typeof<int list>)] (fun [value] -> <@@ Ast.Value.IntVector (%%value : int list) @@>)
            ptyp
        | Ast.ValueType.Pair (typ1, typ2) ->
            let ptyp1 = getProvidedType(None, typ1)
            let ptyp2 = getProvidedType(None, typ2)
            let ptyp =
                let name =
                    sprintf "PairOf%sAnd%s" ptyp1.Name ptyp2.Name
                    |> defaultArg name
                    |> newName
                new ProvidedTypeDefinition(name, Some (typeof<Ast.Value>))
            let propTyp = typedefof<_*_>.MakeGenericType(ptyp1, ptyp2)
            addProperty ptyp ("Value", propTyp) (fun this -> <@@ (%%this : Ast.Value).GetPair() @@>)
            addConstructor ptyp [("Value", propTyp)] (fun [value] -> <@@ Ast.Value.Pair (%%value : Ast.Value * Ast.Value) @@>)
            ptyp
        | Ast.ValueType.Triplet (typ1, typ2, typ3) ->
            let ptyp1 = getProvidedType(None, typ1)
            let ptyp2 = getProvidedType(None, typ2)
            let ptyp3 = getProvidedType(None, typ3)
            let ptyp =
                let name =
                    sprintf "TripletOf%sAnd%sAnd%s" ptyp1.Name ptyp2.Name ptyp3.Name
                    |> defaultArg name
                    |> newName
                new ProvidedTypeDefinition(name, Some (typeof<Ast.Value>))
            let propTyp = typedefof<_*_*_>.MakeGenericType(ptyp1, ptyp2, ptyp3)
            addProperty ptyp ("Value", propTyp) (fun this -> <@@ (%%this : Ast.Value).GetTriplet() @@>)
            addConstructor ptyp [("Value", propTyp)] (fun [value] -> <@@ Ast.Value.Triplet (%%value : Ast.Value * Ast.Value * Ast.Value) @@>)
            ptyp
        | Ast.ValueType.Date ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Date", Some (typeof<Ast.Value>))
            addProperty ptyp ("Year", typeof<int>) (fun this ->
                let e = <@@ (%%this : Ast.Value).GetDate() @@>
                <@@ let _, _, year = (%%e : int * int * int) in year @@>)
            addProperty ptyp ("Month", typeof<int>) (fun this ->
                let e = <@@ (%%this : Ast.Value).GetDate() @@>
                <@@ let _, month, _ = (%%e : int * int * int) in month @@>)
            addProperty ptyp ("Day", typeof<int>) (fun this ->
                let e = <@@ (%%this : Ast.Value).GetDate() @@>
                <@@ let day, _, _ = (%%e : int * int * int) in day @@>)
            addConstructor ptyp [("Day", typeof<int>); ("Month", typeof<int>); ("Year", typeof<int>)] (fun [day; month; year] ->
                <@@ Ast.Value.Date((%%day : int), (%%month : int), (%%year : int)) @@>)
            ptyp
        | Ast.ValueType.Composite fields ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Composite" |> newName, Some (typeof<Ast.Value>))
            let asList this = <@ (%%this : Ast.Value).GetItems() @>
            let getters =
                fields
                |> Map.map (
                    fun fieldName (def, minMult, maxMult) ->
                        let fieldType =
                            let subName =
                                match def with
                                | Ast.ValueType.Set _
                                | Ast.ValueType.Mapping _
                                | Ast.ValueType.Composite _ -> Some fieldName
                                | _ -> None
                            getProvidedType(subName, def)
                        match (minMult, maxMult) with
                        | Ast.MinMultiplicity.MinOne, Ast.MaxMultiplicity.MaxOne ->
                            let prop = new ProvidedProperty(fieldName, fieldType)
                            prop.GetterCode <-
                                fun [this] ->
                                    let e = asList this
                                    <@@
                                        match List.tryFind (fun (name, _) -> name = fieldName) %e with
                                        | Some (_, value) -> value
                                        | None -> failwithf "Field '%s' is not set" fieldName
                                    @@>
                            prop
                        | Ast.MinMultiplicity.Zero, Ast.MaxOne ->
                            let optTyp =
                                typeof<option<_>>
                                    .GetGenericTypeDefinition()
                                    .MakeGenericType(fieldType)
                            let prop = new ProvidedProperty(fieldName, optTyp)
                            prop.GetterCode <-
                                fun [this] ->
                                    let e = asList this
                                    <@@
                                        match List.tryFind (fun (name, _) -> name = fieldName) %e with
                                        | Some (_, value) -> Some value
                                        | None -> None
                                    @@>
                            prop                                    
                        | _, Ast.MaxMultiplicity.Multiple ->
                            let listTyp =
                                typeof<List<_>>
                                    .GetGenericTypeDefinition()
                                    .MakeGenericType(fieldType)
                            let prop = new ProvidedProperty(fieldName, listTyp)
                            prop.GetterCode <-
                                fun [this] ->
                                    let e = asList this
                                    <@@
                                        List.filter (fun (name, _) -> name = fieldName) %e
                                    @@>
                            prop
                    )
                |> Map.toList
                |> List.sortBy fst
                |> List.map snd
            ptyp.AddMembers(getters)
            // setters, using fluent interfaces
            for kvp in fields do
                let fieldName = kvp.Key
                let (def, minMult, maxMult) = kvp.Value
                let fieldType =
                    let subName =
                        match def with
                        | Ast.ValueType.Set _
                        | Ast.ValueType.Mapping _
                        | Ast.ValueType.Composite _ -> Some fieldName
                        | _ -> None
                    getProvidedType(subName, def)
                match (minMult, maxMult) with
                | Ast.MinMultiplicity.MinOne, Ast.MaxMultiplicity.MaxOne ->
                    addMethod
                        ptyp
                        ((sprintf "Set%s" fieldName), ptyp)
                        [("value", upcast fieldType)]
                        (fun [this; value] ->
                            <@@
                                let this = (%%this : Ast.Value)
                                this.SetItem(fieldName, (%%value : Ast.Value))
                            @@>)
                | Ast.MinMultiplicity.Zero, Ast.MaxOne ->
                    let optTyp =
                        typedefof<option<_>>
                            .MakeGenericType(fieldType)
                    addMethod
                        ptyp
                        ((sprintf "Set%s" fieldName), ptyp)
                        [("value", optTyp)]
                        (fun [this; value] ->
                            <@@
                                let this = (%%this : Ast.Value)
                                this.SetItem(fieldName, (%%value : Ast.Value option))
                            @@>)
                | _, Ast.MaxMultiplicity.Multiple ->
                    let listTyp =
                        typedefof<List<_>>
                            .MakeGenericType(fieldType)
                    addMethod
                        ptyp
                        ((sprintf "Set%s" fieldName), ptyp)
                        [("value", listTyp)]
                        (fun [this; value] ->
                            <@@
                                let this = (%%this : Ast.Value)
                                this.ClearItems(fieldName).AddItems(fieldName, (%%value : Ast.Value list))
                            @@>)
            // Create Mcu instances
            match Mcu.tryMkAsCommand typ with
            | Some _ ->
                addMethod
                    ptyp
                    ("AsCommand", typeof<Mcu.McuCommand>)
                    []
                    (fun [this] ->
                        <@@
                            match Mcu.tryMkAsCommand %typExpr with
                            | Some f -> f (%%this : Ast.Value)
                            | None -> failwith "Unexpected error: could not build AsCommand"
                        @@>)
            | None -> ()
            match Mcu.tryMkAsEntity typ with
            | Some _ ->
                addMethod
                    ptyp
                    ("AsEntity", typeof<Mcu.McuEntity>)
                    []
                    (fun [this] ->
                        <@@
                            match Mcu.tryMkAsEntity %typExpr with
                            | Some f -> f (%%this : Ast.Value)
                            | None -> failwith "Unexpected error: could not build AsEntity"
                        @@>)
            | None -> ()
            match Mcu.tryMkAsHasEntity typ with
            | Some _ ->
                addMethod
                    ptyp
                    ("AsHasEntity", typeof<Mcu.HasEntity>)
                    []
                    (fun [this] ->
                        <@@
                            match Mcu.tryMkAsHasEntity %typExpr with
                            | Some f -> f (%%this : Ast.Value)
                            | None -> failwith "Unexpected error: could not build AsHasEntity"
                        @@>)
            | None -> ()

            ptyp
        | Ast.ValueType.Mapping itemTyp ->
            let ptyp1 = getProvidedType(None, itemTyp)
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Mapping" |> newName, Some (typeof<Ast.Value>))
            addConstructor ptyp [] (fun [] -> <@@ Ast.Value.Mapping [] @@>)
            let propTyp = typedefof<Map<_,_>>.MakeGenericType(typeof<int>, ptyp1)
            addProperty ptyp ("Value", propTyp) (fun this -> <@@ (%%this : Ast.Value).GetMapping() |> Map.ofList @@>)
            addMethod ptyp ("SetItem", ptyp) [("Key", typeof<int>); ("Value", upcast ptyp1)] (fun [this; key; value] ->
                <@@
                    let this = (%%this : Ast.Value)
                    this.SetItem((%%key : int), (%%value : Ast.Value))
                @@>)
            addMethod ptyp ("RemoveItem", ptyp) (["Key", typeof<int>]) (fun [this; key] ->
                <@@
                    let this = (%%this : Ast.Value)
                    this.RemoveItem(%%key : int)
                @@>)
            addMethod ptyp ("Clear", ptyp) [] (fun [this] ->
                <@@
                    let this = (%%this : Ast.Value)
                    this.Clear()
                @@>)
            ptyp
        | Ast.ValueType.Set itemTyp ->
            let ptyp1 = getProvidedType(None, itemTyp)
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Set" |> newName, Some (typeof<Ast.Value>))
            addConstructor ptyp [] (fun [] -> <@@ Ast.Value.Set [] @@>)
            let propTyp = typedefof<Set<_>>.MakeGenericType(ptyp1)
            addProperty ptyp ("Value", propTyp) (fun this -> <@@ (%%this : Ast.Value).GetSet() |> Set.ofList @@>)
            ptyp

    and getProvidedType (name, typ) : ProvidedTypeDefinition =
        cached cache (buildProvidedType) (name, typ)

    getProvidedType, cache


let buildParserType(namedValueTypes : (string * Ast.ValueType * ProvidedTypeDefinition) list) =
    let parser = ProvidedTypeDefinition("Parser", Some typeof<IDictionary<Ast.ValueType, Parsing.ParserFun>>)
    let valueTypeExprs =
        namedValueTypes
        |> List.map (fun (_, x, _) -> x)
        |> List.fold (fun expr valueType ->
            let valueTypeExpr = valueType.ToExpr()
            <@ %valueTypeExpr :: %expr @>
            ) <@ [] @>
    // Default constructor: Populate the cache of parsers.
    addConstructor parser [] (fun [] ->
        <@@
            %valueTypeExprs
            |> List.map(fun valueType -> (valueType, Parsing.makeParser valueType))
            |> dict
        @@>)
    // Parse methods for all top types.
    for name, valueType, ptyp in namedValueTypes do
        let vtExpr = valueType.ToExpr()
        let retType =
            typedefof<_*_>.MakeGenericType(ptyp, typeof<Parsing.Stream>)
        addMethod parser (sprintf "Parse_%s" name, retType) [("s", typeof<Parsing.Stream>)] (fun [this; s] ->
            <@@
                let parsers = (%%this : IDictionary<Ast.ValueType, Parsing.ParserFun>)
                let parser = parsers.[%vtExpr]
                parser.Run(%%s : Parsing.Stream)
            @@>
        )
    parser


let buildGroupParserType(namedValueTypes : (string * Ast.ValueType * ProvidedTypeDefinition) list) =
    let parser = ProvidedTypeDefinition("GroupData", Some typeof<Ast.Data list>)
    let valueTypeOfName =
        namedValueTypes
        |> List.fold (fun expr (name, valueType, _) ->
            <@
                Map.add name %(valueType.ToExpr()) %expr
            @>
            ) <@ Map.empty @>
    // Constructor: Parse a group or mission file
    addConstructor parser [("s", typeof<Parsing.Stream>)] (fun [s] ->
        <@@
            let parsers =
                %valueTypeOfName
                |> Map.map (fun name valueType -> Parsing.makeParser valueType)
            let getParser name = parsers.[name]
            let s = (%%s : Parsing.Stream)
            Parsing.parseFile getParser s
        @@>)
    // Getters: list of objects of each type
    for (name, valueType, ptyp) in namedValueTypes do
        addProperty parser ((sprintf "ListOf%s" name), typedefof<_ list>.MakeGenericType(ptyp)) (fun this ->
            <@@
                let this = (%%this : Ast.Data list)
                let ret =
                    this
                    |> List.map (fun data -> data.GetLeaves())
                    |> List.concat
                    |> List.choose (function (name2, value) -> if name2 = name then Some value else None)
                ret
            @@>)
    // Return result
    parser


[<TypeProvider>]
type MissionTypes(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "SturmovikMissionTypes"

    let provider = ProvidedTypeDefinition(asm, ns, "Provider", Some(typeof<obj>))
    let sampleParam = ProvidedStaticParameter("sample", typeof<string>)
    do sampleParam.AddXmlDoc("<brief>Name of a mission file from which the structure of missions is infered</brief>")

    do provider.DefineStaticParameters([sampleParam], fun typeName [| sample |] ->
        let sample = sample :?> string
        if not(System.IO.File.Exists(sample)) then
            failwithf "Cannot open sample file '%s' for reading" sample
        let ty = new ProvidedTypeDefinition(asm, ns, typeName, Some(typeof<obj>))
        // The types corresponding to the ValueTypes extracted from the sample file
        let getProvidedType, cache = mkProvidedTypeBuilder(ty)
        let types, _ = AutoSchema.getTopTypes(Parsing.Stream.FromFile(sample))
        for t in types do
            ignore <| getProvidedType(Some t.Key, t.Value)
        let types =
            cache
            |> Seq.map (fun kvp -> kvp.Value)
            |> List.ofSeq
        ty.AddMembers(types)
        // The type of the parser.
        let namedTypes =
            cache
            |> Seq.choose (fun kvp ->
                match fst kvp.Key with
                | Some name -> Some (name, snd kvp.Key, kvp.Value)
                | None -> None)
            |> List.ofSeq
        let parserType = buildParserType namedTypes
        ty.AddMember(parserType)
        // The type of the file parser.
        let parserType = buildGroupParserType namedTypes
        ty.AddMember(parserType)
        // Result
        ty
    )

    do this.AddNamespace(ns, [provider])

[<assembly:TypeProviderAssembly>]
do()
