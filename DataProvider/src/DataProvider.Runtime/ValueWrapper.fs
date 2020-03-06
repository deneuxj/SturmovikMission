﻿//    Copyright 2015, 2020 Johann Deneux
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

/// The base type of all provided types representing objects found in a mission file, wraps an Ast.Value
type AstValueWrapper(value : SturmovikMission.DataProvider.Ast.Value) =
    member this.Wrapped = value

/// The base type of the result of parsing a mission file
type GroupMembers(items : SturmovikMission.DataProvider.Ast.Data list) =
    member this.Items = items