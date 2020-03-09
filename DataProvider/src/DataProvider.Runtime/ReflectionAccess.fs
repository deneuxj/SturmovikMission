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

namespace SturmovikMission.Util

open System.Reflection

[<AutoOpen>]
module internal Constants =
    /// Binding flags to access to public static members.
    let internal reflAccessBind = BindingFlags.Public ||| BindingFlags.Static

/// A static class to provide easy access to standard F# functions that I find hard to get a hold on otherwise.
type ReflectionAccess =
    /// Direct access to Map.map wrapped in a method.
    static member MapMap(fn, xs) = xs |> Map.map (fun _ -> fn)

    /// Get the method info reflection data from a static method in this class
    static member GetStaticMethod(name) = typeof<ReflectionAccess>.GetMethod(name, reflAccessBind)