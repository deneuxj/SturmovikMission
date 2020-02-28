//    Copyright 2019 Johann Deneux
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

module SturmovikMission.DataProvider.UniqueNames

open System.Collections.Generic

type UniqueNames() =
    let names = new HashSet<string>()

    member this.NewName(name : string) =
        let nameSeq =
            Seq.initInfinite (fun i -> if i = 0 then name else sprintf "%s_%d" name i)
        let uniqueName = nameSeq |> Seq.find (fun name -> not(names.Contains(name)))
        names.Add(uniqueName) |> ignore
        uniqueName