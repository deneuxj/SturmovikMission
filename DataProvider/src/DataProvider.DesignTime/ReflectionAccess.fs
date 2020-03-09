﻿namespace SturmovikMission.Util

open System.Reflection

[<AutoOpen>]
module internal Constants =
    let internal reflAccessBind = BindingFlags.Public ||| BindingFlags.Static

type ReflectionAccess =
    static member MapMap<'TK, 'T1, 'T2 when 'TK : comparison>(fn : 'T1 -> 'T2, xs : Map<'TK, 'T1>) = Map.map (fun _ x -> fn x) xs

    static member GetStaticMethod(name) = typeof<ReflectionAccess>.GetMethod(name, reflAccessBind)