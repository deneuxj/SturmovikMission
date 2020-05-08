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

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open System.Collections.Generic
open System
open System.IO
open SturmovikMission.Cached
open SturmovikMission.DataProvider.UniqueNames
open SturmovikMission.DataProvider
open SturmovikMission.ProvidedDataBuilder

module internal Internal =
    open SturmovikMission.Expr.ExprExtensions
    open SturmovikMission.Expr.AstExtensions
    open UncheckedQuotations

    [<AutoOpen>]
    module AstValueWrapperTypeBuildingHelpers =
        type IProvidedDataBuilder with
            /// Create a type inheriting from AstValueWrapper, with a constructor taking an Ast.Value
            member this.NewWrapper(name) =
                let ptyp = this.NewType(name, typeof<AstValueWrapper>)
                let constructor = this.NewConstructor(["value", typeof<Ast.Value>], fun _ -> <@@ () @@>)
                constructor.BaseConstructorCall <-
                    function
                    | this :: value :: _ ->
                        let cinfo = typeof<AstValueWrapper>.GetConstructor [| typeof<Ast.Value> |]
                        let args = [this; value]
                        cinfo, args
                    | _ -> failwith "Wrong number of arguments passed to AstValueWrapper constructor"
                ptyp.AddMember(constructor)
                ptyp

            /// Create a named constructor, i.e. a public static method to create a new instance
            member this.NewNamedConstructor(name, ptyp : ProvidedTypeDefinition, args, body) =
                let fn =
                    match args with
                    | [] -> fun expr -> this.NewStaticProperty(name, ptyp, expr []) :> System.Reflection.MemberInfo
                    | _ :: _ -> fun body -> this.NewStaticMethod(name, ptyp, args, body) :> System.Reflection.MemberInfo
                let constructor =
                    fn <|
                        fun args ->
                            let value : Expr<Ast.Value> = body args
                            let constructor =
                                ptyp.GetConstructors()
                                |> Array.find(fun cinfo ->
                                    match cinfo.GetParameters() with
                                    | [| typ |] -> typ.ParameterType.FullName = typeof<Ast.Value>.FullName
                                    | _ -> false)
                            Expr.NewObject (
                                constructor,
                                [value]
                            )

                constructor

            /// Help to create a constructor. Takes a setup function that returns an Ast.Value
            member this.NewConstructor(args, setup) =
                let constructor = this.NewConstructor(args, fun _ -> <@@ () @@>)
                constructor.BaseConstructorCall <-
                    function
                    | this :: args ->
                        let cinfo = typeof<AstValueWrapper>.GetConstructor [| typeof<Ast.Value> |]
                        let value : Expr<Ast.Value> = setup args
                        let args = [this; value.Raw]
                        cinfo, args
                    | _ -> failwith "Wrong number of arguments passed to AstValueWrapper constructor"
                constructor

            member this.NewProperty(name, typ, body : Expr<Ast.Value> -> Expr) =
                let body this =
                    let wrapper = Expr.Convert<AstValueWrapper>(this)
                    let value =
                        <@ (%wrapper).Wrapped @>
                    body value
                this.NewProperty(name, typ, body)

            member this.NewMethod(name, typ, args, body : Expr<Ast.Value> -> Expr list -> Expr) =
                let body args =
                    match args with
                    | [] ->
                        failwithf "Missing 'this' argument to instance-bound method '%s'" name
                    | that :: args ->
                        let wrapper = Expr.Convert<AstValueWrapper>(that)
                        let value =
                            <@ (%wrapper).Wrapped @>
                        body value args
                this.NewMethod(name, typ, args, body)

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

    let (|GroundType|ComplexType|) kind =
        if Ast.isGroundType kind then GroundType else ComplexType

    /// Create a function that can build an Expr that constructs an instance of a generated type, given an Expr representing an Ast.Value
    let wrap (fieldType : ProvidedTypeDefinition) =
        let constructor = fieldType.GetConstructor([| typeof<Ast.Value> |])
        fun (e : Expr<Ast.Value>) -> Expr.NewObjectUnchecked(constructor, [ e ])

    /// <summary>
    /// Build the function that builds ProvidedTypeDefinitions for ValueTypes encountered in the sample mission file.
    /// </summary>
    /// <param name="top">Definition of the top type in the type provider.</param>
    let mkProvidedTypeBuilder logInfo (pdb : IProvidedDataBuilder) =
        logInfo "Started inferring all MCU value types"

        let cache = new Dictionary<TypeIdentification, ProvidedTypeDefinition>(HashIdentity.Structural)

        let asList this = <@ (%%this : Ast.Value).GetItems() @>

        /// Add static property AstType to a generated type. It returns the ValueType.
        let addAstValueTypeProperty (ptyp : ProvidedTypeDefinition, vt : Ast.ValueType) =
            ptyp.AddMember(pdb.NewStaticProperty("AstType", typeof<Ast.ValueType>, vt.ToExpr()))

        // Builders for the ground types

        let ptypBoolean =
            let ptyp = pdb.NewWrapper("Boolean")
            ptyp.AddMember(pdb.NewProperty("Value", typeof<bool>, fun this -> <@@ (%this : Ast.Value).GetBool() @@>))
            ptyp.AddMember(pdb.NewNamedConstructor("N", ptyp, [("value", typeof<bool>)], fun args -> let value = args.[0] in <@ Ast.Value.Boolean (%%value : bool) @>))
            ptyp.AddMember(pdb.NewNamedConstructor("Default", ptyp, [], fun _ -> <@ Ast.Value.Boolean false @>))
            addAstValueTypeProperty(ptyp, Ast.ValueType.Boolean)
            ptyp

        let ptypFloat =
            let ptyp = pdb.NewWrapper("Float")
            ptyp.AddMember(pdb.NewProperty("Value", typeof<float>, fun this -> <@@ (%this : Ast.Value).GetFloat() @@>))
            ptyp.AddMember(pdb.NewNamedConstructor("N", ptyp, [("value", typeof<float>)], fun args -> let value = args.[0] in <@ Ast.Value.Float (%%value : float) @>))
            ptyp.AddMember(pdb.NewNamedConstructor("Default", ptyp, [], fun _ -> <@ Ast.Value.Float 0.0 @>))
            addAstValueTypeProperty(ptyp, Ast.ValueType.Float)
            ptyp

        let ptypFloatPair =
            let ptyp = pdb.NewWrapper("FloatPair")
            ptyp.AddMember(pdb.NewProperty("Value", typeof<float * float>, fun this -> <@@ (%this : Ast.Value).GetFloatPair() @@>))
            ptyp.AddMember(pdb.NewNamedConstructor("N", ptyp, [("value", typeof<float * float>)], fun args -> let value = args.[0] in <@ let x, y = (%%value : float * float) in Ast.Value.FloatPair(x, y) @>))
            ptyp.AddMember(pdb.NewNamedConstructor("Default", ptyp, [], fun _ -> <@ Ast.Value.FloatPair(0.0, 0.0) @>))
            addAstValueTypeProperty(ptyp, Ast.ValueType.FloatPair)
            ptyp

        let ptypInteger =
            let ptyp = pdb.NewWrapper("Integer")
            ptyp.AddMember(pdb.NewProperty("Value", typeof<int>, fun this -> <@@ (%this : Ast.Value).GetInteger() @@>))
            ptyp.AddMember(pdb.NewNamedConstructor("N", ptyp, [("value", typeof<int>)], fun args -> let value = args.[0] in <@ Ast.Value.Integer (%%value : int) @>))
            ptyp.AddMember(pdb.NewNamedConstructor("Default", ptyp, [], fun _ -> <@ Ast.Value.Integer 0 @>))
            addAstValueTypeProperty(ptyp, Ast.ValueType.Integer)
            ptyp

        let ptypMask =
            let ptyp = pdb.NewWrapper("Mask")
            ptyp.AddMember(pdb.NewProperty("Value", typeof<int64>, fun this -> <@@ (%this : Ast.Value).GetMask() @@>))
            ptyp.AddMember(pdb.NewNamedConstructor("N", ptyp, [("value", typeof<int64>)], fun args -> let value = args.[0] in <@ Ast.Value.Mask (%%value : int64) @>))
            ptyp.AddMember(pdb.NewNamedConstructor("Default", ptyp, [], fun _ -> <@ Ast.Value.Mask 0L @>))
            addAstValueTypeProperty(ptyp, Ast.ValueType.Mask)
            ptyp

        let ptypString =
            let ptyp = pdb.NewWrapper("String")
            ptyp.AddMember(pdb.NewProperty("Value", typeof<string>, fun this -> <@@ (%this : Ast.Value).GetString() @@>))
            ptyp.AddMember(pdb.NewNamedConstructor("N", ptyp, [("value", typeof<string>)], fun args -> let value = args.[0] in <@ Ast.Value.String (%%value : string) @>))
            ptyp.AddMember(pdb.NewNamedConstructor("Default", ptyp, [], fun _ -> <@ Ast.Value.String "" @>))
            addAstValueTypeProperty(ptyp, Ast.ValueType.String)
            ptyp

        let ptypIntVector =
            let ptyp = pdb.NewWrapper("VectorOfIntegers")
            ptyp.AddMember(pdb.NewProperty("Value", typeof<int list>, fun this -> <@@ (%this : Ast.Value).GetIntVector() @@>))
            ptyp.AddMember(pdb.NewNamedConstructor("N", ptyp, [("value", typeof<int list>)], fun args -> let value = args.[0] in <@ Ast.Value.IntVector (%%value : int list) @>))
            ptyp.AddMember(pdb.NewNamedConstructor("Default", ptyp, [], fun _ -> <@ Ast.Value.IntVector [] @>))
            addAstValueTypeProperty(ptyp, Ast.ValueType.IntVector)
            ptyp

        let ptypDate =
            let ptyp = pdb.NewWrapper("Date")
            ptyp.AddMember(pdb.NewProperty("Year", typeof<int>, fun this ->
                let e = <@@ (%this : Ast.Value).GetDate() @@>
                <@@ let _, _, year = (%%e : int * int * int) in year @@>))
            ptyp.AddMember(pdb.NewProperty("Month", typeof<int>, fun this ->
                let e = <@@ (%this : Ast.Value).GetDate() @@>
                <@@ let _, month, _ = (%%e : int * int * int) in month @@>))
            ptyp.AddMember(pdb.NewProperty("Day", typeof<int>, fun this ->
                let e = <@@ (%this : Ast.Value).GetDate() @@>
                <@@ let day, _, _ = (%%e : int * int * int) in day @@>))
            ptyp.AddMember(
                pdb.NewNamedConstructor(
                    "FromDate",
                    ptyp,
                    [("Day", typeof<int>); ("Month", typeof<int>); ("Year", typeof<int>)],
                    function
                    | [day; month; year] ->
                        <@ Ast.Value.Date((%%day : int), (%%month : int), (%%year : int)) @>
                    | _ -> failwith "Unmatched list of parameters and list of arguments"))
            addAstValueTypeProperty(ptyp, Ast.ValueType.Date)
            ptyp

        let addComplexNestedType(ptyp : ProvidedTypeDefinition, subpTyp : ProvidedTypeDefinition, kind) =
            match kind with
            | GroundType ->
                ()
            | ComplexType ->
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
                    | Ast.ValueType.Mask -> ptypMask
                    | Ast.ValueType.String -> ptypString
                    | Ast.ValueType.IntVector -> ptypIntVector
                    | Ast.ValueType.Date -> ptypDate
                    | Ast.ValueType.Pair (typ1, typ2) -> buildPair(typId, typ1, typ2)
                    | Ast.ValueType.Triplet (typ1, typ2, typ3) -> buildTriple(typId, typ1, typ2, typ3)
                    | Ast.ValueType.Composite fields ->
                        let typExpr = typId.Kind.ToExpr()
                        let ptyp = pdb.NewWrapper(name)
                        let parents = name :: typId.Parents
                        // Add types of complex fields as nested types
                        for field in fields do
                            let fieldName = field.Key
                            let fieldKind, _, _ = field.Value
                            let subpTyp = getProvidedType { Name = fieldName; Kind = fieldKind; Parents = parents }
                            addComplexNestedType(ptyp, subpTyp, fieldKind)
                        // Getters
                        ptyp.AddMembers(getters parents fields)
                        // Setters
                        ptyp.AddMembers(setters parents (fields, ptyp))
                        // Create as MCU
                        ptyp.AddMembers(asMcu (name, typId.Kind, typExpr))
                        // Parse
                        ptyp.AddMember(staticParser (typId.Kind, name, ptyp))
                        // Dump to text
                        let meth = pdb.NewMethod("AsString", typeof<string>, [], fun this _ ->
                            <@@
                                sprintf "%s %s" name (Ast.dump (%this : Ast.Value))
                            @@>)
                        ptyp.AddMember(meth)
                        // Result
                        ptyp
                    | Ast.ValueType.Mapping itemTyp ->
                        let subName = sprintf "%s_ValueType" name
                        let ptyp1 = getProvidedType { Name = subName; Kind = itemTyp; Parents = name :: typId.Parents }
                        let ptyp = pdb.NewWrapper(name)
                        addComplexNestedType(ptyp, ptyp1, itemTyp)
                        // Constructor from map
                        ptyp.AddMember(
                            pdb.NewNamedConstructor("FromMap", ptyp, [("map", ProvidedTypeBuilder.MakeGenericType(typedefof<Map<_, _>>, [typeof<int>; ptyp1]))], fun args ->
                            let m = Expr.ConvertMap<int, AstValueWrapper> args.[0]
                            <@
                                let m =
                                    (%m)
                                    |> Map.map (fun _ (wrapper : AstValueWrapper) -> wrapper.Wrapped)
                                Ast.Value.Mapping(Map.toList m)
                            @>))
                        // Value getter
                        let propTyp = ProvidedTypeBuilder.MakeGenericType(typedefof<Map<_,_>>, [typeof<int>; ptyp1])
                        ptyp.AddMember(
                            pdb.NewProperty(
                                "Value", 
                                propTyp,
                                fun this ->
                                    let values =
                                        <@
                                            (%this : Ast.Value).GetMapping()
                                            |> Map.ofList
                                        @>
                                    let wrap = wrap ptyp1
                                    let wrapped = Expr.MapMap(ptyp1, values, wrap)
                                    Expr.Coerce(wrapped, propTyp)))
                        // Set item in the map
                        ptyp.AddMember(pdb.NewMethod("SetItem", ptyp, [("Key", typeof<int>); ("Value", upcast ptyp1)], fun this args ->
                            match args with
                            | [key; value] ->
                                let value = Expr.Convert<AstValueWrapper>(value)
                                <@@
                                    let this = (%this : Ast.Value)
                                    this.SetItem((%%key : int), (%value).Wrapped)
                                @@>
                            | _ ->
                                failwith "Unmatched list of parameters and arguments"))
                        // Remove item from the map
                        ptyp.AddMember(pdb.NewMethod("RemoveItem", ptyp, ["Key", typeof<int>], fun this args ->
                            let key = args.[0]
                            <@@
                                let this = (%this : Ast.Value)
                                this.RemoveItem(%%key : int)
                            @@>))
                        // Clear map
                        ptyp.AddMember(pdb.NewMethod("Clear", ptyp, [], fun this _ ->
                            <@@
                                let this = (%this : Ast.Value)
                                this.Clear()
                            @@>))
                        // Result
                        ptyp
                    | Ast.ValueType.List itemTyp ->
                        let subName = sprintf "%s_ValueType" name
                        let ptyp1 = getProvidedType { Name = subName; Kind = itemTyp; Parents = name :: typId.Parents }
                        let ptyp = pdb.NewWrapper(name)
                        addComplexNestedType(ptyp, ptyp1, itemTyp)
                        // Value getter
                        let propTyp = ProvidedTypeBuilder.MakeGenericType(typedefof<IEnumerable<_>>, [ptyp1])
                        ptyp.AddMember(
                            pdb.NewProperty(
                                "Value",
                                propTyp,
                                fun this ->
                                    let values =
                                        <@
                                            (%this : Ast.Value).GetList()
                                            :> IEnumerable<Ast.Value>
                                        @>
                                    let wrap = wrap ptyp1
                                    Expr.MapItems(ptyp1, values, wrap)))
                        // constructor with value
                        ptyp.AddMember(
                            pdb.NewNamedConstructor(
                                "FromList",
                                ptyp,
                                ["items", propTyp],
                                fun args ->
                                    let items = Expr.ConvertEnumerable<AstValueWrapper> args.[0]
                                    <@
                                        %items
                                        |> Seq.map (fun w -> w.Wrapped)
                                        |> List.ofSeq
                                        |> Ast.Value.List
                                    @>
                            )
                        )
                        // Result
                        ptyp
                // Add a default constructor and the value type property, unless it's a ground type (it's already got those)
                if not (Ast.isGroundType typId.Kind) then
                    let kind = typId.Kind.ToExpr()
                    ptyp.AddMember(pdb.NewNamedConstructor("Default", ptyp, [], fun _ -> <@ Ast.defaultValue %kind @>))
                    addAstValueTypeProperty(ptyp, typId.Kind)
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
            let ptyp = pdb.NewWrapper(typId.Name)
            addComplexNestedType(ptyp, ptyp1, typ1)
            addComplexNestedType(ptyp, ptyp2, typ2)
            // Value getter
            let propTyp = ProvidedTypeBuilder.MakeTupleType [ptyp1; ptyp2]
            ptyp.AddMember(
                pdb.NewProperty(
                    "Value", 
                    propTyp, 
                    fun this ->
                        let tp = <@@ (%this : Ast.Value).GetTriplet() @@>
                        Expr.NewTuple [
                            Expr.TupleGetUnchecked(tp, 0) |> Expr.Cast<Ast.Value> |> wrap ptyp1
                            Expr.TupleGetUnchecked(tp, 1) |> Expr.Cast<Ast.Value> |> wrap ptyp2
                        ]
                )
            )
            // Constructor
            ptyp.AddMember(
                pdb.NewNamedConstructor(
                    "Create",
                    ptyp,
                    [("Item1", ptyp1 :> Type); ("Item2", upcast ptyp2)],
                    fun args ->
                        let item1 = Expr.Convert<AstValueWrapper>(args.[0])
                        let item2 = Expr.Convert<AstValueWrapper>(args.[1])
                        <@ Ast.Value.Pair ((%item1).Wrapped, (%item2).Wrapped) @>))
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
            let ptyp = pdb.NewWrapper(name)
            addComplexNestedType(ptyp, ptyp1, typ1)
            addComplexNestedType(ptyp, ptyp2, typ2)
            addComplexNestedType(ptyp, ptyp3, typ3)
            let propTyp = ProvidedTypeBuilder.MakeTupleType([ptyp1; ptyp2; ptyp3])
            // Value getter
            ptyp.AddMember(
                pdb.NewProperty(
                    "Value",
                    propTyp,
                    fun this ->
                        let tp = <@@ (%this : Ast.Value).GetTriplet() @@>
                        Expr.NewTuple [
                            Expr.TupleGetUnchecked(tp, 0) |> Expr.Cast<Ast.Value> |> wrap ptyp1
                            Expr.TupleGetUnchecked(tp, 1) |> Expr.Cast<Ast.Value> |> wrap ptyp2
                            Expr.TupleGetUnchecked(tp, 2) |> Expr.Cast<Ast.Value> |> wrap ptyp3
                        ]
                )
            )
            // Constructor
            ptyp.AddMember(
                pdb.NewNamedConstructor(
                    "Create",
                    ptyp,
                    [("Item1", ptyp1 :> Type); ("Item2", upcast ptyp2); ("Item3", upcast ptyp3)],
                    fun args ->
                        let item1 = Expr.Convert<AstValueWrapper>(args.[0])
                        let item2 = Expr.Convert<AstValueWrapper>(args.[1])
                        let item3 = Expr.Convert<AstValueWrapper>(args.[2])
                        <@ Ast.Value.Triplet ((%item1).Wrapped, (%item2).Wrapped, (%item3).Wrapped) @>))
            // Result
            ptyp

        // Build the getters in composite types
        and getters parents fields =
            fields
            |> Map.map (
                fun fieldName (def, minMult, maxMult) ->
                    let fieldType =
                        getProvidedType { Name = fieldName; Kind = def; Parents = parents }

                    let wrap = wrap fieldType
                    let mkReturnExpr (e : Expr<Ast.Value>) =
                        Expr.Coerce(wrap e, fieldType)

                    match (minMult, maxMult) with
                    | Ast.MinMultiplicity.MinOne, Ast.MaxMultiplicity.MaxOne ->
                        pdb.NewMethod(
                            sprintf "Get%s" fieldName,
                            fieldType,
                            [],
                            fun this _ ->
                                let e = asList this
                                <@
                                    match List.tryFind (fun (name, _) -> name = fieldName) %e with
                                    | Some (_, value) -> value
                                    | None -> failwithf "Field '%s' is not set" fieldName
                                @>
                                |> mkReturnExpr)
                    | Ast.MinMultiplicity.Zero, Ast.MaxOne ->
                        let optTyp =
                            ProvidedTypeBuilder.MakeGenericType(
                                typedefof<_ option>,
                                [fieldType])
                        pdb.NewMethod(
                            sprintf "TryGet%s" fieldName,
                            optTyp,
                            [],
                            fun this _ ->
                                let fields = asList this
                                let fieldName = Expr.Value(fieldName) |> Expr.Cast<string>
                                let value =
                                    <@
                                        %fields
                                        |> List.tryPick (fun (name, x) -> if name = %fieldName then Some x else None)
                                    @>
                                let outOpt = Expr.MapOption(fieldType, value, wrap)
                                Expr.Coerce(outOpt, optTyp)
                        )
                    | _, Ast.MaxMultiplicity.Multiple ->
                        let listTyp =
                            ProvidedTypeBuilder.MakeGenericType(
                                typedefof<IEnumerable<_>>,
                                [fieldType])
                        pdb.NewMethod(
                            sprintf "Get%ss" fieldName,
                            listTyp,
                            [],
                            fun this _ ->
                                let fields = asList this
                                let fieldName = Expr.Value(fieldName) |> Expr.Cast<string>
                                let values =
                                    <@
                                        %fields
                                        |> List.choose (fun (name, x) -> if name = %fieldName then Some x else None)
                                        :> IEnumerable<Ast.Value>
                                    @>
                                Expr.MapItems(fieldType, values, wrap)
                        )
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
                        [("value", fieldType :> Type)],
                        fun this args ->
                            let value = Expr.Convert<AstValueWrapper>(args.[0])
                            <@@
                                let this2 = (%this).SetItem(fieldName, (%value).Wrapped)
                                AstValueWrapper(this2)
                            @@>)
                | Ast.MinMultiplicity.Zero, Ast.MaxOne ->
                    let optTyp = ProvidedTypeBuilder.MakeGenericType(
                                    typedefof<_ option>,
                                    [fieldType])
                    pdb.NewMethod(
                        sprintf "Set%s" fieldName,
                        ptyp,
                        [("value", optTyp)],
                        fun this args ->
                            let value = args.[0]
                            let value = Expr.ConvertOpt<AstValueWrapper>(value)
                            <@@
                                let arg = 
                                    %value
                                    |> Option.map (fun w -> w.Wrapped)
                                let this2 = (%this).SetItem(fieldName, arg)
                                AstValueWrapper(this2)
                            @@>)
                | _, Ast.MaxMultiplicity.Multiple ->
                    let listTyp =
                        ProvidedTypeBuilder.MakeGenericType(
                            typedefof<_ list>,
                            [fieldType])
                    pdb.NewMethod(
                        sprintf "Set%s" fieldName,
                        ptyp,
                        [("value", listTyp)],
                        fun this args ->
                            let value = Expr.ConvertEnumerable<AstValueWrapper>(args.[0])
                            <@@
                                let xs = (%value) |> Seq.map (fun wrapper -> wrapper.Wrapped) |> List.ofSeq
                                AstValueWrapper((%this).ClearItems(fieldName).AddItems(fieldName, xs))
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
                            fun this _ ->
                                <@@
                                    match McuFactory.tryMakeMcu(name, %typExpr) with
                                    | Some f -> f((%this : Ast.Value), [])
                                    | None -> failwith "Unexpected error: could not build MCU"
                                @@>)
                        |> addXmlDoc """<summary>Create a new mutable instance of an MCU.</summary>"""
                | None -> ()
            ]

        // static method to create a parser
        and staticParser (valueType : Ast.ValueType, name, ptyp) =
            let vtExpr = valueType.ToExpr()
            let retType = typeof<Parsing.ParserFun>

            pdb.NewStaticMethod("GetParser", retType, [], fun _ ->
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
            |> Seq.map (fun (name, vt, _) -> name, vt)
            |> Map.ofSeq
            |> Ast.ValueType.MapToExpr

        let method =
            pdb.NewMethod("CreateMcuList", typeof<Mcu.McuBase list>, [], fun (args : Expr list) ->
                let this = Expr.Convert<GroupMembers>(args.[0])
                <@@
                    let this = %this
                    let valueTypeOfName = %valueTypeOfName
                    let mcuMakerOfName =
                        valueTypeOfName
                        |> Map.map (fun name vt ->
                            McuFactory.tryMakeMcu(name, vt))
                    this.Items
                    |> List.collect (fun data -> data.GetLeavesWithPath())
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
        let dataListType = ProvidedTypeBuilder.MakeGenericType(typedefof<_ list>, [typeof<Ast.Data>])
        let parser =
            pdb.NewType("GroupData", typeof<GroupMembers>)
            |> addXmlDoc """Extraction of data from a mission or group file."""
        let valueTypeOfName =
            namedValueTypes
            |> Seq.map (fun (name, vt, _) -> name, vt)
            |> Map.ofSeq
            |> Ast.ValueType.MapToExpr
        // Constructor: From a list of AST nodes
        let constructFromList =
            let constructor =
                pdb.NewConstructor([("nodes", dataListType)], fun _ -> <@@ () @@>)
            constructor.BaseConstructorCall <-
                function
                | this :: nodes :: _ ->
                    let cinfo = typeof<GroupMembers>.GetConstructor [| typeof<Ast.Data list> |]
                    cinfo, [this; nodes]
                | _ ->
                    failwith "Wrong number of arguments"
            constructor
            |> addXmlDoc """
                <summary>Provide access to parsed data.</summary>
                <param name="nodes">The result of parsing a group or mission file</param>"""
        parser.AddMember(constructFromList)
        let callConstructor (arg : Expr<Ast.Data list>) =
            Expr.NewObject(constructFromList, [arg])
        // Static method: Parse a group or mission file
        parser.AddMember(
            let constructor =
                pdb.NewStaticMethod("Parse", parser, [("s", typeof<Parsing.Stream>)],
                    function
                    | s :: _ ->
                        let nodes =
                            <@
                                let parsers =
                                    %valueTypeOfName
                                    |> Map.map (fun name valueType -> Parsing.makeParser valueType)
                                let getParser name = parsers.[name]
                                let s = (%%s : Parsing.Stream)
                                let data = Parsing.parseFile getParser s
                                data
                            @>
                        callConstructor nodes
                    | _ ->
                        failwith "Wrong number of arguments")
            constructor
            |> addXmlDoc """
                <summary>Parse a mission or group file and store the extracted data.</summary>
                <param name="s">The stream that is parsed</param>
                <exception cref="Parsing.ParseError">Failed to parse the mission or group</exception>""")
        // Get data from a subgroup
        parser.AddMember(
            pdb.NewMethod("GetGroup", parser, [("name", typeof<string>)], 
                function
                | [this; name] ->
                    let this = Expr.Convert<GroupMembers>(this)
                    let nodes =
                        <@
                            let nodes = (%this).Items
                            nodes
                            |> List.map (fun node -> node.FindByPath [(%%name : string)])
                            |> List.concat
                        @>
                    callConstructor nodes
                | _ -> failwith "Unmatched list of parameters and list of arguments"
            )
            |> addXmlDoc """
                <summary>Get data from a subgroup</summary>
                <param name="name">Name of the subgroup</param>""")
        // Getters: list of objects of each type
        for (name, valueType, ptyp) in topComplexTypes do
            let wrap = wrap ptyp
            parser.AddMember(
                pdb.NewProperty(sprintf "ListOf%s" name, ProvidedTypeBuilder.MakeGenericType(typedefof<IEnumerable<_>>, [ptyp]), fun this ->
                    let this = Expr.Convert<GroupMembers>(this)
                    let values =
                        <@
                            let ret =
                                (%this).Items
                                |> Seq.collect (fun data -> data.GetLeaves())
                                |> Seq.choose (function (name2, value) -> if name2 = name then Some value else None)
                            ret :> IEnumerable<_>
                        @>
                    Expr.MapItems(ptyp, values, wrap))
                |> addXmlDoc (sprintf """<summary>Build a list of immutable instances of %s</summary>""" name))
        // Get the flattened list of objects as instances of McuBase and its subtypes, when appropriate
        parser.AddMember(
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

    let buildProvider (enableLogging : bool) (typeName : string, sample : string) =
        if not(String.IsNullOrWhiteSpace(System.Environment.GetEnvironmentVariable("TP_DEBUG"))) then
            System.Diagnostics.Debugger.Launch() |> ignore

        let asm = ProvidedAssembly()
        let logInfo, closeLog =
            if enableLogging then
                let logger = Logging.initLogging() |> Result.bind Logging.openLogFile
                Logging.log logger, fun() -> Logging.closeLog logger
            else
                (fun _ -> ()), (fun () -> ())

        let invokeImpl =
            if config.IsHostedExecution then
                InvokeCodeImplementation.AsProvided
            else
                InvokeCodeImplementation.FromAssembly

        logInfo(sprintf "Provider invoked with sample '%s' using invokeCodeImpl '%s'" sample (string invokeImpl))

        let pdb = IProvidedDataBuilder.CreateWithEmptyShells invokeImpl
        let ty = new ProvidedTypeDefinition(asm, ns, typeName, Some(typeof<obj>), isErased=false)
        // The types corresponding to the ValueTypes extracted from the sample file
        let getProvidedType, cache = Internal.mkProvidedTypeBuilder logInfo pdb
        let types, _ = AutoSchema.getTopTypes(Parsing.Stream.FromFile(sample))
        // Add top types
        let topTypeDefs =
            types
            |> Seq.map (fun kvp -> getProvidedType { Name = kvp.Key; Kind = kvp.Value; Parents = [] })
            |> List.ofSeq
        ty.AddMembers topTypeDefs
        // Add ground types
        let groundTypeDefs =
            Seq.zip Ast.groundValueTypes Ast.groundValueTypeNames
            |> Seq.map (fun (kind, name) -> getProvidedType { Name = name; Kind = kind; Parents = [] })
            |> List.ofSeq
        ty.AddMembers(groundTypeDefs)
        // All types, but filter out cache entries for fields with ground types
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
        // Add the root type to provided assembly
        asm.AddTypes [ty]
        // Result
        ty, modifs, closeLog

    // Cache the top provided type definitions.
    let cache = new Dictionary<(string * string), ProvidedTypeDefinition * (FileWithTime.File list) * (unit -> unit) >(HashIdentity.Structural)
    let getProvider logInfo = cached cache (buildProvider logInfo)

    let provider = ProvidedTypeDefinition(asm, ns, "Provider", Some(typeof<obj>), isErased=false)
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

    do provider.DefineStaticParameters([sampleParam; enableLoggingParam], fun typeName staticArgs ->
        let sample, enableLogging =
            match staticArgs with
            | [| sample; enableLogging |] -> sample, enableLogging
            | _ -> failwith "Wrong number of arguments to type provider"
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
