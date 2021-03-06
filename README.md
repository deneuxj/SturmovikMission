# README #

A set of F# libraries to parse and manipulate mission files in IL-2: Battle of Stalingrad.
Unless specified otherwise, these libraries and their source code are licensed under the GNU Lesser Public License, see COPYING.txt and COPYING.LESSER.txt

## Directory content ##

This solution is composed of the following projects:

* DataProvider/CodeGen: Code generator tool that produces types inferred from a sample mission file

* McuLibrary: A library with mutable types to manipulate nodes of which missions are composed.

All the interesting code can be divided in four layers:

* Support code: ProvidedTypes.fsi, ProvidedTypes.fs, Cached.fs

* Parsing that produces a dynamically typed AST: Ast.fs, Parsing.fs, Unification.fs, AutoSchema.fs

* Utilities in project McuLibrary to facilitate manipulation of parsed data: Mcu.fs, NumericalIdentifiers.fs

* Code generation tool: Type provider on top of the parsing modules: CodeGenerator.fs

In the list above, layers mentioned early are used by layers mentioned later

## How do I get set up? ##

Once:
```
cd DataProvider
dotnet tool restore
dotnet paket update
```

Building:

```
dotnet build -c release
dotnet test -c release
```

Most people won't need to do this, but should you want to produce the nuget packages of the code generator and the runtime library:

```
dotnet pack -v release
```

## How do I use this to build missions? ##

You can use the parsing layer with the manipulation layer directly, or through the generated code.

### Using direct access to the Abstract Syntax Tree ###

First, you will need to extract data schema from a sample file, and then build the parser for the mission or group files you will be working with.

```fsharp
let types, _ = AutoSchema.getTopTypes (Parsing.Stream.FromFile "Sample.mission")
let parsers = types |> Map.map (fun name typ -> Parsing.makeParser typ)
```

Parse data from a mission file:

```fsharp
let data = Parsing.parseFile (fun name -> parsers.[name]) (Parsing.Stream.FromFile "MyMission.mission")
```

At this point, you have mission data represented in trees of type Ast.Value, which are immutable. You can combine them in any way you see fit, producing new trees. Every tree can be dumped as a string that conforms to the mission file format:

```fsharp
let value : Ast.Value = ...
let s = Ast.dump value
```

Instances of Ast.Value are dynamically typed, which means that of you attempt to use a field that does not exist or has the wrong type, you will discover your error first when you run your mission-building code (if you are lucky). To help with this issue, a type provider is available to convey compile and edit-time type safety.

### Using code generation ###

Install the code generation tool:
```bash
dotnet tool install SturmovikMission.CodeGen
```

Run the code generator:
```bash
dotnet tool run codegen-il2 -o SturmovikMissionTypes.fs -n T
```

This should generate a file SturmovikMissionTypes.fs that defines a namespace named T. Add that file at the top of your project.

Add the libraries used by the generated code:

```bash
dotnet add package FsPickler
dotnet add package SturmovikMission.McuLibrary
```

At this point, T contains a number of types which can be instantiated. Each type has a default constructor, and an explicit constructor with arguments for each non-composite field (i.e. not a list, map...).
There is also a static method ``GetParser`` to create a parser for that type. For instance, the code below creates an airfield tower as an instance of T.Block.

```fsharp
let parser = T.Block.GetParser()
let x3, _ =
    try
        """{
  Name = "My Tower";
  Index = 4;
  LinkTrId = 0;
  XPos = 159106.566;
  YPos = 102.203;
  ZPos = 263544.937;
  XOri = 0.00;
  YOri = 0.00;
  ZOri = 0.00;
  Model = "graphics\blocks\arf_tower_2.mgm";
  Script = "LuaScripts\WorldObjects\Blocks\arf_tower_2.txt";
  Country = 0;
  Desc = "";
  Durability = 25000;
  DamageReport = 50;
  DamageThreshold = 1;
  DeleteAfterDeath = 1;
}"""
        |> Stream.FromString
        |> parser.Run
    with
    | :? ParseError as e ->
        printParseError(e) |> String.concat "\n" |> printfn "%s"
        raise e
```

Reading in an entire file is done with T.GroupData. The data it reads can be sorted by their type, and accessed in lists, one list per type:

```fsharp
let groupData =
    try
        T.GroupData(Stream.FromFile @"C:\Users\...\StalingradConquest.Mission")
    with
    | :? ParseError as e ->
        printParseError(e) |> String.concat "\n" |> printfn "%s"
        raise e

let timers = groupData.ListOfMCU_Timer |> List.map...
groupData.ListOfAirfield |> List.iter...
```

Working with data with segregated types might be impractical, especially in a statically-typed language such as F#. There is a family of interfaces that provide access to most objects (commands, blocks, buildings, vehicles, planes...) in a mission. These interfaces are found in module SturmovikMission.DataProvider.Mcu. Instances can be created from the provided types.

From GroupData:
```fsharp
let mcus = groupData.CreateMcuList()
let rabbit =
    mcus
    |> List.pick (function :? McuEntity as ent -> Some ent | _ -> None)

rabbit.OnEvents <- [ { Type = 2; TarId = 123 }; { Type = 3; TarId = 456 } ]
rabbit.Name <- "Rabbit"
rabbit.Pos.Z <- -1.0
rabbit.Ori.X <- 0.1
printfn "%s" (rabbit.AsString())
```

This produces the output below:

```
MCU_TR_Entity {
Index = 1759;
Name = "Rabbit";
Desc = "";
Targets = [1729];
Objects = [];
XPos = 43641.188000;
YPos = 84.329000;
ZPos = -1.000000;
XOri = 0.100000;
YOri = 0.000000;
ZOri = 0.000000;
Enabled = 1;
MisObjID = 1758;
OnEvents
{
OnEvent
{
Type = 2;
TarId = 123;
}
OnEvent
{
Type = 3;
TarId = 456;
}
}
}
```

## Known issues

There is no attempt at backward or forward compatibility by this library as the mission file format evolves with new versions of the game.
Version incompatibility typically manifest itself as a parse errors thrown by this library, or mission loading failures by the game and its tools.

Luckily, this is typically easily fixed by opening the sample mission file passed to this library for schema inference in the mission editor,
if needed add a few nodes taking advantage of the new features, and then save the file.
Rebuild the application, run it again, and it should be able to interact with the new version of the game and its tools.