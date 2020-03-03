namespace SturmovikMission.Util

open System.Reflection

[<AutoOpen>]
module internal Constants =
    let internal reflAccessBind = BindingFlags.Public ||| BindingFlags.Static

type ReflectionAccess =
    static member MapMap(fn, xs) = xs |> Map.map (fun _ -> fn)

    static member GetStaticMethod(name) = typeof<ReflectionAccess>.GetMethod(name, reflAccessBind)