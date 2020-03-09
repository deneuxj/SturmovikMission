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

namespace SturmovikMission.ProvidedDataBuilder

open System
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations

type IProvidedDataBuilder =
    /// Build a ProvidedTypeDefinition
    abstract NewType: string * Type -> ProvidedTypeDefinition
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

[<AutoOpen>]
module ProvidedDataBuilderExtensions =
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

    type IProvidedDataBuilder with
        static member CreateWithEmptyShells(invokeImpl : InvokeCodeImplementation) =
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

            let newType (name : string) (baseType : Type) =
                ProvidedTypeDefinition(name, Some baseType, isErased=false, isSealed=true)

            let newConstructor (args : (string * Type) list) (body : Expr list -> Expr) =
                let args =
                    args
                    |> List.map (fun (n, t) -> ProvidedParameter(n, t))
                let cnstr =
                    ProvidedConstructor(args, funcGate body)
                cnstr

            let newProperty (name : string, typ : Type) (body : Expr -> Expr) =
                let prop = ProvidedProperty(name, typ, fun args -> let this = args.[0] in bodyGate(body this))
                prop

            let newStaticProperty (name : string, typ : Type) (body : Expr) =
                let prop = ProvidedProperty(name, typ, getterCode = (fun _ -> bodyGate body), isStatic = true)
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
                member __.NewType(name, baseType) = newType name baseType
                member __.NewConstructor(args, body) = newConstructor args body
                member __.NewProperty(name, typ, body) = newProperty (name, typ) body
                member __.NewStaticProperty(name, typ, body) = newStaticProperty (name, typ) body
                member __.NewMethod(name, typ, args, body) = newMethod (name, typ) args body
                member __.NewStaticMethod(name, typ, args, body) = newStaticMethod (name, typ) args body
            }
